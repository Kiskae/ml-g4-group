package train

import server._
import agent._

class TrainingGameState(gameProps:GameProperties, physProps:PhysicsProperties,
    lInputProvider:PlayerInputProvider, rInputProvider:PlayerInputProvider)
    extends GameState(gameProps,physProps,lInputProvider,rInputProvider) {
  override def step() = {
    val lInput = lInputProvider.getInput(this, Side.LEFT);
    val rInput = rInputProvider.getInput(this, Side.RIGHT);
    if (!`match`.matchFinished) {
      `match`.step(lInput, rInput)
    }
  }

  override def isFinished() = `match`.matchFinished

  def changeServer() {
    `match`.ball.side = if (`match`.ball.side == Side.LEFT) Side.RIGHT else Side.LEFT
  }
}
