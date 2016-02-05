package train

import agent.PlayerInput
import server._
import qfunc._
import scala.collection.mutable

class BallFollowerRunner(gameProps:GameProperties, physProps:PhysicsProperties) extends Runner {
  val bAgent = new agent.BallFollower(gameProps.playerRadius/2)
  val r = scala.util.Random

  def setupMatch(s:GameState) {
    s.reset()
    bAgent.offset = r.nextInt((2*gameProps.playerRadius/3).toInt)+gameProps.playerRadius/3
    val ball = s.`match`.ball
    ball.pCircle.posX = gameProps.ballRadius + r.nextInt((gameProps.sideWidth-2*gameProps.ballRadius).toInt)
    if (ball.side == Side.LEFT) ball.pCircle.posX *= -1

    s.`match`.lPlayer.pCircle.posX = -(gameProps.playerRadius + r.nextInt((gameProps.sideWidth-2*gameProps.playerRadius).toInt))
  }

  override def run[SType](s:TrainingGameState, qFunc:QFunction[SType], qAgent:QFunctionInputProvider) = {
    val history = mutable.ArrayBuffer[(SType,Int)]()
    val m = s.`match`
    val histMaxSize = 1000000

    s.changeServer()
    do {
      history.clear()
      setupMatch(s)
      while (!m.matchFinished && history.size < histMaxSize) {
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
