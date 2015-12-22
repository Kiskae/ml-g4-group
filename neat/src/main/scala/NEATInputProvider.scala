import agent.{AlwaysLeftInputProvider, PlayerInput}
import neural.NeuralNetwork
import server.GameStateInterface

class NEATInputProvider(val network: NeuralNetwork) extends AlwaysLeftInputProvider {
  override def getInput(gameStateInterface: GameStateInterface): PlayerInput = {
    val ball = gameStateInterface.getBall

    val ballX = (ball.getPosX + 400000) / 800000.0
    val ballY = (ball.getPosY) / (800000.0)

    val playerX = (gameStateInterface.getMe.getPosX + 400000) / 800000.0
    val playerY = (gameStateInterface.getMe.getPosY - 30000) / (250000.0)

    //println(s"Feeding input: ${ballX}, ${ballY}, ${playerX}, ${playerY}")
    network.setInput(ballX, ballY, playerX, playerY)
    network.evaluate

    val Seq(left, right, up) = network.getOutput
//    println(s"Providing output: left = $left, right = $right, up = $up")
    new PlayerInput(left >= 0.5, right >= 0.5, up >= 0.5)
  }
}
