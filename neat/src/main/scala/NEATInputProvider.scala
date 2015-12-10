import agent.{PlayerInput, AlwaysLeftInputProvider}
import org.neuroph.core.NeuralNetwork
import org.neuroph.core.learning.LearningRule
import org.neuroph.nnet.learning.CompetitiveLearning
import server.GameStateInterface

/**
  * Created by bas on 5-12-15.
  */
class NEATInputProvider(neuralNetwork: ENeuralNetwork[LearningRule]) extends AlwaysLeftInputProvider{
  override def getInput(gameStateInterface: GameStateInterface): PlayerInput = {

    val ball = gameStateInterface.getBall

    neuralNetwork.setInput(ball.getPosX, ball.getPosY, ball.getVelX, ball.getVelY)
    neuralNetwork.calculate()

    val output = neuralNetwork.getOutput
    val left = output(0)
    val right = output(1)
    val up = output(2)

    new PlayerInput(left >= 1, right >= 1, up >= 1)


  }
}
