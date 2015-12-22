import agent.{AlwaysLeftInputProvider, PlayerInput}
import neural.NeuralNetwork
import server.GameStateInterface

class NEATInputProvider(val network: NeuralNetwork) extends AlwaysLeftInputProvider {
  override def getInput(gameStateInterface: GameStateInterface): PlayerInput = {
    val ball = gameStateInterface.getBall



    println(s"Feeding input: ${ball.getPosX}, ${ball.getPosY}, ${ball.getVelX}, ${ball.getVelY}")
    network.setInput(ball.getPosX, ball.getPosY, ball.getVelX, ball.getVelY)
    network.evaluate

    val Seq(left, right, up) = network.getOutput
    println(s"Providing output: left = $left, right = $right, up = $up")
    new PlayerInput(left >= 1.0, right >= 1.0, up >= 1.0)
  }
}
