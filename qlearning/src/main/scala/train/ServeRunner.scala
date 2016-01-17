package train

import agent.PlayerInput
import server._
import qfunc._
import scala.collection.mutable

class ServeRunner(gameProps:GameProperties, physProps:PhysicsProperties) extends Runner {
  val r = scala.util.Random

  def setupMatch(s:GameState) {
    s.reset()
    s.`match`.ball.pCircle.posX = -(gameProps.ballRadius + r.nextInt((gameProps.sideWidth-2*gameProps.ballRadius).toInt))
    s.`match`.lPlayer.pCircle.posX = -(gameProps.playerRadius + r.nextInt((gameProps.sideWidth-2*gameProps.playerRadius).toInt))
  }

  override def run[SType](s:carlo.TrainingGameState, qFunc:QFunction[SType], qAgent:QFunctionInputProvider) = {
    val history = mutable.ArrayBuffer[(SType,Int)]()
    val emptyInput = new PlayerInput(false, false, false)
    val m = s.`match`
    val histMaxSize = 1000000

    do {
      history.clear()
      setupMatch(s)
      while (m.ball.pCircle.posX <= 0 && !m.matchFinished && history.size < histMaxSize) {
        val stateNdx = qFunc.stateRepr(s)
        val actionNdx = qAgent.policy(s)
        val lInput = qfunc.QFunctionInputProvider.inputs(actionNdx)

        val rInput = bAgent.getInput(s,Side.RIGHT)

        m.step(lInput, rInput)

        val toInsert = (stateNdx, actionNdx)
        if (history.isEmpty || history.last != toInsert) history += toInsert
      }
    } while (history.size >= histMaxSize)

    val reward = if (s.rScore > 0) -1 else 1

    (reward, history)
  }
}

