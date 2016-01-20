import java.io.{PrintStream, File}
import server._
import agent._
import qfunc._
import scala.util.Try

object UIResultGen extends App {
  def getQAgent(smallDir:File):PlayerInputProvider = {
    val qTable = new AbsoluteQTable(gameProps, physProps)
    qTable.loadFromFile(new File(smallDir.getAbsolutePath+"/QTable.tsv"))
    new QFunctionInputProvider(qTable, randChance=0)
  }

  def getQAgents(largeDir:File):Array[PlayerInputProvider] = {
    println("Loading Q: "+largeDir.getName)
    largeDir.listFiles.map(f => Try(getQAgent(f))).filter(_.isSuccess).map(_.get)
  }

  def getNeatAgent(smallDir:File):PlayerInputProvider = {
    val f = new File(smallDir.getAbsolutePath+"/Network.obj")
    new neat.NEATInputProvider(misc.Persistent.ReadObjectFromFile[neural.NeuralNetwork](f))
  }

  def getNeatAgents(largeDir:File):Array[PlayerInputProvider] = {
    println("Loading NEAT: "+largeDir.getName)
    largeDir.listFiles.map(f => getNeatAgent(f))
    //largeDir.listFiles.map(f => Try(getNeatAgent(f))).filter(_.isSuccess).map(_.get)
  }

  // Does player l beat player r?
  def matchup(l:PlayerInputProvider, r:PlayerInputProvider):Boolean = {
    state.reset()
    val ball = state.`match`.ball
    ball.pCircle.posX = gameProps.ballRadius + rand.nextInt((gameProps.sideWidth-2*gameProps.ballRadius).toInt)
    if (ball.side == Side.LEFT) ball.pCircle.posX *= -1

    var numSteps = 0
    val maxSteps = 1000000

    while (!m.matchFinished && numSteps < maxSteps) {
      val lInput = l.getInput(state,Side.LEFT)
      val rInput = r.getInput(state,Side.RIGHT)
      m.step(lInput, rInput)
      numSteps += 1
    }
    if (numSteps >= maxSteps) { println("maxSteps exceeded") }
    
    state.lScore > 0
  }

  val rand = scala.util.Random

  val gameProps = new GameProperties()
  val physProps = new PhysicsProperties()
  val state = new carlo.TrainingGameState(gameProps, physProps, null, null)
  val m = state.`match`

  case class Trainer(ndx:Int, name:String)
  case class Agent(trainer:Trainer, runNumber:Int, provider:PlayerInputProvider)
  case class Match(server:Agent, reciever:Agent, serverWon:Boolean)

  import scala.collection.mutable
  val trainers = mutable.ArrayBuffer[Trainer]()
  val agents = mutable.ArrayBuffer[Agent]()
  val results = mutable.ArrayBuffer[Match]()

  val ballTrain = Trainer(0, "ballfollower")
  val qTrainRand = Trainer(1, "q_rand_carlo")
  val qTrainComposite = Trainer(2, "q_composite_carlo")
  val qTrainBall = Trainer(3, "q_ballfollower_carlo")
  val neatTrainRand = Trainer(4, "neat_rand")
  val neatTrainBall = Trainer(5, "neat_ball")

  trainers += ballTrain
  agents += Agent(ballTrain, 0, new BallFollower(gameProps.playerRadius/2))
  agents += Agent(ballTrain, 1, new BallFollower(gameProps.playerRadius))

  trainers += qTrainRand
  for ((agent,ndx) <- getQAgents(new File("/mnt/2E06BE9506BE5D91/ML/runs/abstable_rand_carlo")).zipWithIndex) {
    agents += Agent(qTrainRand, ndx, agent)
  }

  trainers += qTrainComposite
  for ((agent,ndx) <- getQAgents(new File("/mnt/2E06BE9506BE5D91/ML/runs/abstable_composite_carlo")).zipWithIndex) {
    agents += Agent(qTrainComposite, ndx, agent)
  }

  trainers += qTrainBall
  for ((agent,ndx) <- getQAgents(new File("/mnt/2E06BE9506BE5D91/ML/runs/abstable_ballfollower_carlo")).zipWithIndex) {
    agents += Agent(qTrainBall, ndx, agent)
  }

  trainers += neatTrainRand
  for ((agent,ndx) <- getNeatAgents(new File("/mnt/2E06BE9506BE5D91/ML/runs/neat_rand")).zipWithIndex) {
  //for ((agent,ndx) <- getNeatAgents(new File("/home/michael/Desktop/neat_rand")).zipWithIndex) {
    agents += Agent(neatTrainRand, ndx, agent)
  }

  trainers += neatTrainBall
  for ((agent,ndx) <- getNeatAgents(new File("/mnt/2E06BE9506BE5D91/ML/runs/neat_ball")).zipWithIndex) {
  //for ((agent,ndx) <- getNeatAgents(new File("/home/michael/Desktop/neat_ball")).zipWithIndex) {
    agents += Agent(neatTrainBall, ndx, agent)
  }

  println("Finished agents")

  for (i <- agents) {
    for (j <- agents) {
      for (_ <- 0 until 500) {
        results += Match(i, j, matchup(i.provider, j.provider))
      }
    }
  }
  println("Finished results")

  for (t <- trainers) {
    println(s"${t.ndx},${t.name}")
  }

  val rOut = new PrintStream(new File("results.tsv"))
  rOut.println("serveTrainNdx,serveSubNdx,recieverTrainNdx,recieverSubNdx,serverWon")
  for (result <- results) {
    rOut.print(s"${result.server.trainer.ndx}\t${result.server.runNumber}\t")
    rOut.print(s"${result.reciever.trainer.ndx}\t${result.reciever.runNumber}\t")
    rOut.println(if (result.serverWon) 1 else 0)
  }
  rOut.close()
}
