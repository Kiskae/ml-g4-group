import agent.{PlayerInputProvider, BallFollower}
import org.neuroph.core.{Neuron, NeuralNetwork}
import org.neuroph.nnet.learning.CompetitiveLearning
import server.{GameProperties, PhysicsProperties, GameState, GameLoop}
import ui.SwingUI

/**
  * Created by bas on 5-12-15.
  */
object NEAT {

  val maxCounter = 5000

  def main(args: Array[String]) = {
    val gameProps = new GameProperties()
    val physProps = new PhysicsProperties()
//    val lInput = new BallFollower(gameProps.playerRadius / 2)

    val neuralNetwork = new NeuralNetwork[CompetitiveLearning]
    neuralNetwork.setInputNeurons(Array(new Neuron(), new Neuron, new Neuron, new Neuron))
    neuralNetwork.setOutputNeurons(Array(new Neuron(), new Neuron, new Neuron))
    val lInput = new NEATInputProvider(neuralNetwork)
    val rInput = new BallFollower(gameProps.playerRadius / 2)
    val s = new GameState(gameProps, physProps, lInput, rInput)
    val generation = new Generation(1, 10)

    while(true) {

      var score = s.getMyScore
      var counter = 0
      while (counter < maxCounter) {
        s.step()

        if (s.getMyScore > score) {
          counter = 0
                  println(s"$counter: $score")
          score = s.getMyScore
        }

        counter = counter + 1
      }

      println(s"Killed prototype. score = $score")
      s.lInputProvider = new NEATInputProvider(neuralNetwork)
    }
  }

}
