package neat

import java.io.File

import agent.{AlwaysLeftInputProvider, PlayerInput}
import misc.Persistent
import neural.NeuralNetwork
import server.GameStateInterface

class NEATInputProvider(val network: NeuralNetwork) extends AlwaysLeftInputProvider {
  override def getInput(gameStateInterface: GameStateInterface): PlayerInput = {
    val ball = gameStateInterface.getBall

    val ballX = (ball.getPosX + 400000) / 800000.0
    val ballY = (ball.getPosY) / (800000.0)

    val playerX = (gameStateInterface.getMe.getPosX + 400000) / 800000.0
    val playerY = (gameStateInterface.getMe.getPosY - 30000) / (250000.0)

    val playerVelX = gameStateInterface.getMe.getVelX / 4500.0
    val playerVelY = gameStateInterface.getMe.getVelY / 15100.0

    //    println(s"Feeding input: ${ballX}, ${ballY}, ${playerX}, ${playerY}")
    network.evaluate(ballX, ballY, playerX, playerY, playerVelX, playerVelY)

    val Seq(left, right, up) = network.getOutput
    //    println(s"Providing output: left = $left, right = $right, up = $up")
    new PlayerInput(left >= 0.5, right >= 0.5, up >= 0.5)
  }
}

object NEATInputProvider {
  def readFromFile(file: File): NEATInputProvider =
    new NEATInputProvider(Persistent.ReadObjectFromFile[NeuralNetwork](file))
}