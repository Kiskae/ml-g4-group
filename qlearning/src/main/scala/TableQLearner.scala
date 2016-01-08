import server._
import agent.{PlayerInput => Input}
import server.{GameStateInterface => State}
import ui.SwingUI
import java.io.{PrintStream, File}
import java.util.Scanner

class TableQLearner(gameProps:GameProperties, physProps:PhysicsProperties, gamma:Double=0.9, var alpha:Double=0.05, var randChance:Double=0.1, batchSize:Int=10) extends AbstractQLearner {
  //Initialize Q
  val qTable = Array.ofDim[Double](30*10*3*3,6)

  //Random number generator
  val r = scala.util.Random

  //Transaction history
  val tHistSize:Int = 1000000
  var tHistCount:Int = 0
  val transHist:CircularBuffer[(Int, Int, Double, Int)] = new CircularBuffer[(Int, Int, Double, Int)](tHistSize)
//  val transHist = Array.ofDim[(Int, Int, Double, Int)](tHistSize)

  def mapX(x:Long) = 10+math.floor(x*10/(gameProps.sideWidth*2.0) min 19 max -10).toInt
  def mapY(y:Long) = (y*10/(gameProps.netHeight*3) min 9 max 0).toInt
  def mapVel(vel:Long) =  1+math.signum(vel / physProps.playerHorizontalSpeed).toInt

  /** Map a multi-dimensional array index to the eqivalent index of a flat array.
    *
    * The values of `nextDim` should range from 0 to `nextDimRange-1`.
    */
  def ndxAddDim(prevNdx:Int, nextDimRange:Int, nextDim:Int) = prevNdx*nextDimRange + nextDim

  def stateNdx(state: State):Int = {
    val ball = state.getBall
    val me = state.getMe

    val diffX = mapX(state.getBall.getPosX - state.getMe.getPosX)
    val diffY = mapY(state.getBall.getPosY - state.getMe.getPosY)
    val velY = mapVel(state.getBall.getVelY)
    val velX = mapVel(state.getBall.getVelX)

    ndxAddDim(ndxAddDim(ndxAddDim(diffX,10,diffY),3,velY),3,velX)
  }

  def qRow(state: State):Array[Double] = {
    qTable(stateNdx(state))
  }

  override def q(state: State, action: Int) = qRow(state)(action)

  override def maxAction(state: State) = {
    val row = qRow(state)
    val max = row.max

    (row.indexOf(max), max)
  }

  override def action(state: State) = {
    val ran = r.nextDouble
    if (ran < randChance)
      r.nextInt(6)
    else
      maxAction(state)._1
  }

  override def updateQ(state: State, action: Int, nextState: State) {

    if (stateNdx(state) == stateNdx(nextState)) return

    //Safe to history, using a modulo seems to be the fastest way.
    transHist.append((stateNdx(state), action, reward(nextState), stateNdx(nextState)))
    if(tHistCount != tHistSize) tHistCount += 1

    //Perform minibatch
    for(i <- 1 to batchSize) {

      val t = transHist(r.nextInt(tHistCount))

      //TODO This canse be refactored by not store these.
      val row = qTable(t._1)
      val rowNext = qTable(t._4)

      //row(action) = (1-alpha)*row(action) + alpha*(reward(nextState) + gamma*maxAction(nextState)._2)
      row(t._2) = (1-alpha)*row(t._2) + alpha*(t._3 + gamma*rowNext.max)
    }

    //Simple annealing will stop at 0.1 ( ~ 10^7 steps until to get to 0.1)
    if (randChance > 0.00000001) randChance = randChance*0.999999992
    if (alpha > 0.000000001) alpha = alpha*0.999999992

  }

  override def reward(state: State) = {
    // if ball coords are y = 0 and 0 > x < 10 than we return 100 else 0.
    if (mapY(state.getBall.getPosY) > 0)
      0
    else if (mapX(state.getBall.getPosX) < 10)
      -1
    else
      1
  }

  def writeToFile(file:File) {
    val stream = new PrintStream(file)
    qTable.foreach {row =>
      row.foreach(x => stream.print(f"$x%6f\t"))
      stream.println()
    }
    stream.close()
  }

  def loadFromFile(file:File) {
    val scanner = new Scanner(file)
    for (i <- 0 until qTable.length) {
      for (j <- 0 until qTable(i).length) {
        qTable(i)(j) = scanner.nextDouble
      }
    }
    scanner.close()
  }
}

object QAgentTrainer extends App {
  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()

  val qAgent = new TableQLearner(gameProps,physProps)
  val bAgent = new agent.BallFollower(gameProps.playerRadius/2)
  val s:GameState = new GameState(gameProps, physProps, qAgent, bAgent);

  val qTableFile = new File("QTable.tsv")
  val trainingEpochs = 1000

  if (qTableFile.exists) {
    qAgent.loadFromFile(qTableFile)
  }

  while (true) {
    s.reset()
    //var hitCount:Long = 0
    var scoreCountL:Long = 0
    var scoreCountR:Long = 0
    1 to trainingEpochs foreach { _ =>
      while (!s.isFinished) {
        s.step()
      }
      //hitCount += s.lHits
      scoreCountL += s.lScore
      scoreCountR += s.rScore
      s.reset()
    }

    println(f"Completed epoch: Alpha: ${qAgent.alpha}%6f  RandC: ${qAgent.randChance}%6f Average score: ${(scoreCountL - scoreCountR).toFloat/trainingEpochs}%6f Q: " + (qAgent.qTable.map(_.sum).sum)/16200)
    //println(f"Completed epoch: Alpha: ${qAgent.alpha}%6f Average score: ${(scoreCountL - scoreCountR).toFloat/trainingEpochs}%6f average hits: ${hitCount.toFloat/trainingEpochs}%6f " + " RandChance: " + qAgent.randChance)

    qAgent.writeToFile(qTableFile)
  }
  println("finished")
}
