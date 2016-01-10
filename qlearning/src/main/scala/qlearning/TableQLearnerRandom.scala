package qlearning

import common.CircularBuffer
import qlearning.AbstractQLearner
import server._
import agent.{PlayerInput => Input}
import server.{GameStateInterface => State}
import ui.SwingUI
import java.io.{PrintStream, File}
import java.util.Scanner

import scala.util.control.Breaks._

class TableQLearnerRandom(gameProps:GameProperties, physProps:PhysicsProperties, gamma:Double=0.9, var alpha:Double=0.001, var randChance:Double=1, batchSize:Int=10) extends AbstractQLearner {
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

  var scoreTemp = 0

  var succesfull = false;
  var failed = false;

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

    if (stateNdx(state) == stateNdx(nextState) && !succesfull && !failed) return

    val row = qRow(state)
    row(action) = (1-alpha)*row(action) + alpha*(reward(nextState) + gamma*maxAction(nextState)._2)

    //Safe to history, using a modulo seems to be the fastest way.
    //    transHist.append((stateNdx(state), action, reward(nextState), stateNdx(nextState)))
    //    if(tHistCount != tHistSize) tHistCount += 1
    //
    //    //Perform minibatch
    //    for(i <- 1 to batchSize) {
    //
    //      val t = transHist(r.nextInt(tHistCount))
    //
    //      //TODO This canse be refactored by not store these.
    //      val row = qTable(t._1)
    //      val rowNext = qTable(t._4)
    //
    //      //row(action) = (1-alpha)*row(action) + alpha*(reward(nextState) + gamma*maxAction(nextState)._2)
    //      row(t._2) = (1-alpha)*row(t._2) + alpha*(t._3 + gamma*rowNext.max)
    //    }

    //Simple annealing will stop at 0.1 ( ~ 10^7 steps until to get to 0.1)
    //if (alpha > 0.000000001) alpha = alpha*0.999999992

  }

  override def reward(state: State) = {
    // if ball coords are y = 0 and 0 > x < 10 than we return 100 else 0.
//    if (mapY(state.getBall.getPosY) > 0)
//      0
//    else if (mapX(state.getBall.getPosX) < 10)
//      -1
//    else
//      1

    //TODO For some reason if i compare getBall.getPosX at this point i do not get the same results as in the main loop. WHY??
    if(succesfull) {
      scoreTemp += 1
      1
    } else if (mapY(state.getBall.getPosY) < 1 || failed) {
      -1
    } else
      0
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

object QAgentTrainerRandom extends App {
  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()

  val qAgent = new TableQLearnerRandom(gameProps,physProps)
  val bAgent = new agent.BallFollower(gameProps.playerRadius/2)
  val s:GameState = new GameState(gameProps, physProps, qAgent, bAgent);

  val qTableFile = new File("QTable.tsv")
  val trainingEpochs = 1000

  if (qTableFile.exists) {
    qAgent.loadFromFile(qTableFile)
  }

  //Random number generator
  val r = scala.util.Random

//      val ui:SwingUI = new SwingUI(gameProps)
//      ui.init(s)

  var epochCounter = 0
  var score2 = 0

  while (epochCounter < 5000) {
    s.reset()
    //var hitCount:Long = 0
    var scoreCountL:Long = 0
    var scoreCountR:Long = 0


    1 to trainingEpochs foreach { i =>

//            s.`match`.ball.pCircle.posX = 0
//            s.`match`.ball.pCircle.posY = 220000 + 100000
//            s.`match`.ball.pCircle.velX = -7000
//            s.`match`.ball.pCircle.velY = -1000
//            s.`match`.ball.firstHit = false

      s.`match`.ball.pCircle.posX = 0
      s.`match`.ball.pCircle.posY = 220000 + r.nextInt(350000)
      s.`match`.ball.pCircle.velX = -r.nextInt(15000)
      s.`match`.ball.pCircle.velY = r.nextInt(10000) - 5000
      s.`match`.ball.firstHit = false

      s.`match`.lPlayer.pCircle.posX = -r.nextInt(400000 - 30000) + 15000
      s.`match`.lPlayer.pCircle.posY = r.nextInt(235000) + 15000

      breakable {
        while (!s.isFinished) {

          s.step()

          if (s.getBall.getPosX > 0) {

            if(s.getMyHits > 0) {
              qAgent.succesfull = true
              score2 += 1
            }

            s.step()
            qAgent.succesfull = false
            break
          }

          if (qAgent.mapY(s.getBall.getPosY) < 1) { qAgent.failed = true; s.step(); qAgent.failed=false; break;}

//          if (epochCounter > 80) {
//          ui.display(s)
//          Thread.sleep(20)
//          }

        }
      }

      //hitCount += s.lHits
      scoreCountL += s.lScore
      scoreCountR += s.rScore

      s.reset()

    }

    //if (qAgent.randChance > 0.00000001) qAgent.randChance = qAgent.randChance*0.91

    println(f"Completed epoch: Alpha: ${qAgent.alpha}%6f  RandC: ${qAgent.randChance}%6f Average score: ${(qAgent.scoreTemp)}%6f:${score2} Q: " + (qAgent.qTable.map(_.sum).sum)/16200)
    //println(f"Completed epoch: Alpha: ${qAgent.alpha}%6f Average score: ${(scoreCountL - scoreCountR).toFloat/trainingEpochs}%6f average hits: ${hitCount.toFloat/trainingEpochs}%6f " + " RandChance: " + qAgent.randChance)

    qAgent.scoreTemp = 0
    score2 = 0
    epochCounter += 1
    qAgent.writeToFile(qTableFile)
  }
  println("finished")
}
