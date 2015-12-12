import agent.{AlwaysLeftInputProvider, PlayerInput}
import org.neuroph.core.NeuralNetwork
import server.GameStateInterface

class NEATInputProvider(val network: NeuralNetwork[_]) extends AlwaysLeftInputProvider {
  override def getInput(gameStateInterface: GameStateInterface): PlayerInput = {
    val ball = gameStateInterface.getBall

    network.setInput(ball.getPosX, ball.getPosY, ball.getVelX, ball.getVelY)
    network.calculate()

    val Array(left, right, up) = network.getOutput
    new PlayerInput(left >= 1, right >= 1, up >= 1)
  }
}
