import java.io.{PrintStream, File}
import server._
import agent._
import qfunc._
import scala.util.Try

object ResultGen extends App {
  def getAgent(smallDir:File, abs:Boolean):PlayerInputProvider = {
    val qTable = if (!abs) new QTable(gameProps, physProps)
                 else new AbsoluteQTable(gameProps, physProps)
    qTable.loadFromFile(new File(smallDir.getAbsolutePath+"/QTable.tsv"))
    new QFunctionInputProvider(qTable, randChance=0)
  }

  def getAgents(largeDir:File):Array[PlayerInputProvider] = {
    val absolute = largeDir.getName.startsWith("abs")
    println(largeDir.getName)
    largeDir.listFiles.map(f => Try(getAgent(f,absolute))).filter(_.isSuccess).map(_.get)
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

  // How many times does a player from a beat a player from b?
  def matchup2(a:Array[PlayerInputProvider], b:Array[PlayerInputProvider]):(Int,Int) = {
    var winCount = 0
    var matchCount = 0
    for (i <- a) {
      for (j <- b) {
        if (matchup(i,j)) winCount += 1
        if (!matchup(j,i)) winCount += 1
        matchCount += 2
      }
    }

    (winCount,matchCount)
  }

  require(args.length == 1, "Must take one arg: <runDir>")
  val runDir = new File(args(0))
  val files = runDir.listFiles
  val rand = scala.util.Random

  val gameProps = new GameProperties()
  val physProps = new PhysicsProperties()
  val state = new carlo.TrainingGameState(gameProps, physProps, null, null)
  val m = state.`match`

  val agentNames = files.map(_.getName)
  val agents = files.map(getAgents(_))
  println(agents.map(_.size).sum)

  println("Finished agents")

  case class Trainer(ndx:Int, qtable:String, runner:String, trainer:String)
  case class Agent(trainer:Trainer, runNumber:Int, provider:PlayerInputProvider)
  case class Match(server:Agent, reciever:Agent, serverWon:Boolean)

  import scala.collection.mutable
  val trainers = mutable.ArrayBuffer[Trainer]()
  val agentsC = mutable.ArrayBuffer[Agent]()
  val results = mutable.ArrayBuffer[Match]()
 
  for (i <- 0 until agents.size) {
    val name = agentNames(i).split("_")
    val trainer = Trainer(i, name(0),name(1),name(2))
    trainers += trainer
    for (j <- 0 until agents(i).size) {
      agentsC += Agent(trainer, j+1, agents(i)(j))
    }
  }

  for (i <- agentsC) {
    for (j <- agentsC) {
      for (_ <- 0 until 1000) {
        results += Match(i, j, matchup(i.provider, j.provider))
      }
    }
  }
  println("Finished results")

  val tOut = new PrintStream(new File("trainers.csv"))
  tOut.println("ndx,qType,runType,trainType")
  for (t <- trainers) {
    tOut.println(s"${t.ndx},${t.qtable},${t.runner},${t.trainer}")
  }
  tOut.close()

  val rOut = new PrintStream(new File("results.tsv"))
  rOut.println("serveTrainNdx,serveSubNdx,recieverTrainNdx,recieverSubNdx,serverWon")
  for (result <- results) {
    rOut.print(s"${result.server.trainer.ndx}\t${result.server.runNumber}\t")
    rOut.print(s"${result.reciever.trainer.ndx}\t${result.reciever.runNumber}\t")
    rOut.println(if (result.serverWon) 1 else 0)
  }
  rOut.close()
}
