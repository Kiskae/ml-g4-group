package train

import agent.PlayerInput
import server._
import qfunc._
import scala.collection.mutable
import math._

class MeanRandRunner(gameProps:GameProperties, physProps:PhysicsProperties, meanChance:Double=0.4) extends RandRunner(gameProps, physProps) {
  val meanExamples = mutable.ArrayBuffer[(Long,Long,Long,Long,Long)]()
  val histMaxSize = 10000
  val emptyInput = new PlayerInput(false, false, false)

  def loadSetup(s:GameState, setup:(Long,Long,Long,Long,Long)) {
    s.reset()
    val ball = s.`match`.ball
    val pc = ball.pCircle
    pc.posX = setup._1
    pc.posY = setup._2
    pc.velY = setup._3
    pc.velX = setup._4
    s.`match`.lPlayer.pCircle.posX = setup._5
    ball.firstHit = false
  }

  override def run[SType](s:TrainingGameState, qFunc:QFunction[SType], qAgent:QFunctionInputProvider) = {
    if (meanExamples.size > 0 && r.nextDouble < meanChance) {
      runMean(s,qFunc,qAgent)
    } else {
      runRand(s,qFunc,qAgent)
    }
  }

  def runRand[SType](s:TrainingGameState, qFunc:QFunction[SType], qAgent:QFunctionInputProvider) = {
    // (stateNdx,actionNdx)
    val history = mutable.ArrayBuffer[(SType,Int)]()
    var setup = getSetup(s)

    do {
      history.clear()
      setupMatch(s)
      setup = getSetup(s)
      runMatch(s,history,qFunc,qAgent)
    } while ((s.lHits == 0 && s.rScore == 0) || history.size >= histMaxSize)

    val reward = if (s.rScore > 0) -1 else 1
    if (reward < 0) meanExamples += setup

    (reward, history)
  }

  def runMean[SType](s:TrainingGameState, qFunc:QFunction[SType], qAgent:QFunctionInputProvider) = {
    // (stateNdx,actionNdx)
    val history = mutable.ArrayBuffer[(SType,Int)]()
    val setupNdx = r.nextInt(meanExamples.size)

    loadSetup(s, meanExamples(setupNdx))
    runMatch(s,history,qFunc,qAgent)

    if (history.size >= histMaxSize) history.clear()

    val reward = if (s.rScore > 0) -1 else 1
    if (reward > 0) {
      meanExamples(setupNdx) = meanExamples.last
      meanExamples.trimEnd(1)
    }

    (reward, history)
  }

  def runMatch[SType](s:TrainingGameState, history:mutable.ArrayBuffer[(SType,Int)], qFunc:QFunction[SType], qAgent:QFunctionInputProvider) {
    val m = s.`match`
    var crossNet = false
    while ((!crossNet || m.ball.pCircle.posX <= 0) && !m.matchFinished && history.size < histMaxSize) {
      crossNet |= m.ball.pCircle.posX <= 0
      val stateNdx = qFunc.stateRepr(s)
      val actionNdx = qAgent.policy(s)
      val lInput = qfunc.QFunctionInputProvider.inputs(actionNdx)

      val rInput = emptyInput

      m.step(lInput, rInput)

      val toInsert = (stateNdx, actionNdx)
      if (history.isEmpty || history.last != toInsert) history += toInsert
    }
  }
}
