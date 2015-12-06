import agent.{PlayerInputProvider, BallFollower}
import org.neuroph.core.learning.LearningRule
import org.neuroph.core.{Neuron, NeuralNetwork}
import org.neuroph.nnet.learning.CompetitiveLearning
import server.{GameProperties, PhysicsProperties, GameState, GameLoop}
import ui.SwingUI

/**
  * Created by bas on 5-12-15.
  */
object NEAT {

  val maxIdleSteps = 10000

  def main(args: Array[String]) = {

    val neuralNetwork = new NeuralNetwork[LearningRule]
    neuralNetwork.setInputNeurons(Array(new Neuron(), new Neuron, new Neuron, new Neuron))
    neuralNetwork.setOutputNeurons(Array(new Neuron(), new Neuron, new Neuron))
    val lInput = new NEATInputProvider(neuralNetwork)
//    val lInput = new BallFollower(30000L / 2)
    val rInput = new BallFollower(30000L /  2)
    val generation = new Generation(1, 10)

    //TODO for each NN in generation: run 1 "game" and evaluate.

    val score = evaluate(neuralNetwork, lInput, rInput)
    println(s"Killed prototype. score = $score")

//    s.lInputProvider = new NEATInputProvider(neuralNetwork)
  }

  def evaluate(neuralNetwork: NeuralNetwork[LearningRule], lInput : PlayerInputProvider, rInput : PlayerInputProvider) = {
    val gameProps = new GameProperties()
    val physProps = new PhysicsProperties()
    val s = new GameState(gameProps, physProps, lInput, rInput)
    var latestScore = s.getMyScore
    var stepCounter = 0
    var latestPointGainedStep = 0

    while ((stepCounter - latestPointGainedStep) < maxIdleSteps) {
      s.step()

      if (s.getMyScore > latestScore) {
        latestPointGainedStep = stepCounter
        latestScore = s.getMyScore
        println(s"$stepCounter: $latestScore")
      }

      stepCounter = stepCounter + 1
    }

    latestScore
  }

}
