import server._
import agent._

class InfiniteLoopGameState(gameProps:GameProperties, physProps:PhysicsProperties, lInputProvider:PlayerInputProvider, rInputProvider:PlayerInputProvider) extends GameState(gameProps, physProps, lInputProvider, rInputProvider){
  class InfiniteLoopMatchState(gameProps:GameProperties, physProps:PhysicsProperties) extends MatchState(gameProps, physProps) {
    lazy val r = scala.util.Random
    override def reset() {
      super.reset()
      val pc = ball.pCircle
      pc.posX=239279
      pc.posY=210710
      pc.velY=5016
      pc.velX=14243
    }
  }

  `match` = new InfiniteLoopMatchState(gameProps, physProps)
  `match`.addObserver(this)
  `match`.reset()
}
