package train

import agent.PlayerInput
import server._
import qfunc._
import scala.collection.mutable

class CenterRandRunner(gameProps:GameProperties, physProps:PhysicsProperties) extends Runner {
  val r = scala.util.Random

  def setupMatch(s:GameState) {
    s.reset()
    val ball = s.`match`.ball
    val pc = ball.pCircle
    pc.posX = 0
    pc.posY = gameProps.netHeight + gameProps.ballRadius + r.nextInt(3*gameProps.netHeight.toInt)
    pc.velX = -r.nextInt(10000)-1
    pc.velY = 10000-r.nextInt(20000)
    ball.firstHit = false
  }

  override def run[SType](s:carlo.TrainingGameState, qFunc:QFunction[SType], qAgent:QFunctionInputProvider) = {
    // (stateNdx,actionNdx)
    val history = mutable.ArrayBuffer[(SType,Int)]()
    val emptyInput = new PlayerInput(false, false, false)
    val m = s.`match`
    val histMaxSize = 1000000

    do {
      history.clear()
      setupMatch(s)
      while (m.ball.pCircle.posX <= 0 && s.rScore == 0 && history.size < histMaxSize) {
        val stateNdx = qFunc.stateRepr(s)
        val actionNdx = qAgent.policy(s)
        val lInput = qfunc.QFunctionInputProvider.inputs(actionNdx)

        val rInput = emptyInput

        m.step(lInput, rInput)

        val toInsert = (stateNdx, actionNdx)
        if (history.isEmpty || history.last != toInsert) history += toInsert
      }
    } while ((s.lHits == 0 && s.rScore == 0) || history.size >= histMaxSize)

    val reward = if (s.rScore > 0) -1 else 1

    (reward, history)
  }
}
