import server._
import agent._

class RandomInitGameState(gameProps:GameProperties, physProps:PhysicsProperties, lInputProvider:PlayerInputProvider, rInputProvider:PlayerInputProvider) extends GameState(gameProps, physProps, lInputProvider, rInputProvider){
  class RandomInitMatchState(gameProps:GameProperties, physProps:PhysicsProperties) extends MatchState(gameProps, physProps) {
    lazy val r = scala.util.Random
    override def reset() {
      super.reset()
      val ball = `match`.ball
      ball.pCircle.posX = gameProps.ballRadius + r.nextInt((gameProps.sideWidth-2*gameProps.ballRadius).toInt)
      if (ball.side == Side.LEFT) ball.pCircle.posX *= -1
    }
  }

  `match` = new RandomInitMatchState(gameProps, physProps)
  `match`.addObserver(this)
  `match`.reset()
}
