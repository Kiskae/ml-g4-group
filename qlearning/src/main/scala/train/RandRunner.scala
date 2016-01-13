package train

import agent.PlayerInput
import server._
import qfunc._
import scala.collection.mutable
import math._

class RandRunner(gameProps:GameProperties, physProps:PhysicsProperties) extends Runner {
  val r = scala.util.Random

  def setupMatch(s:GameState) {
    s.reset()
    val ball = s.`match`.ball
    val pc = ball.pCircle
    pc.posX = gameProps.ballRadius + r.nextInt((gameProps.sideWidth-2*gameProps.ballRadius).toInt)
    pc.posY = physProps.playerMaxHeight/4 + r.nextInt(3*physProps.playerMaxHeight.toInt/4)

    val angle = r.nextDouble*Pi
    pc.velY = round(physProps.playerCollisionVelocity*sin(angle))
    pc.velX = round(physProps.playerCollisionVelocity*cos(angle))

    s.`match`.lPlayer.pCircle.posX = -(gameProps.ballRadius + r.nextInt((gameProps.sideWidth-2*gameProps.ballRadius).toInt))
    ball.firstHit = false
  }

  def getSetup(s:GameState) = {
    val pc = s.`match`.ball.pCircle
    (pc.posX,pc.posY,pc.velY,pc.velX,s.`match`.lPlayer.pCircle.posX)
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
      val setup = getSetup(s)
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
      if (history.size >= histMaxSize) {
        println(s"setup:$setup hits:${s.lHits}")
      }
    } while ((s.lHits == 0 && s.rScore == 0) || history.size >= histMaxSize)

    val reward = if (s.rScore > 0) -1 else 1

    (reward, history)
  }
}
