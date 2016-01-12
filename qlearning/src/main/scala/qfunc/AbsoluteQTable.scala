package qfunc

import server._
import server.{GameStateInterface => State, GameProperties => GameProps, PhysicsProperties => PhysProps}

class AbsoluteQTable(gameProps:GameProps, physProps:PhysProps, alpha:Double = 0.1) extends QTable(gameProps, physProps, alpha=alpha) {
  override lazy val table = Array.ofDim[Double](11*10*20*10*3*3,6)

  override def mapVel(vel:Long) =  1+math.signum(vel / (physProps.playerHorizontalSpeed/2)).toInt

  override def stateRepr(state: GameStateInterface):Int = {
    val ball = state.getBall
    val me = state.getMe

    var out = if (me.getVelY == 0) 0 else 1+mapY(me.getPosY)
    out = ndxAddDim(out, 10, mapX(me.getPosX))
    out = ndxAddDim(out, 20, mapX(ball.getPosX))
    out = ndxAddDim(out, 10, mapY(ball.getPosY))
    out = ndxAddDim(out, 3, mapVel(ball.getVelX))
    out = ndxAddDim(out, 3, mapVel(ball.getVelY))

    out
  }

  override def qRow(stateRepr: Int): Array[Double] = ???
}
