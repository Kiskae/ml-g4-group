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

//    val lInput = new BallFollower(30000L / 2)
    val rInput = new BallFollower(30000L /  2)
    val generation = new Generation(1, 10, 4, 3)
    generation.initialize()

    //TODO for each NN in generation: run 1 "game" and evaluate.

    for(neuralNetwork <- generation.getAllNetworks()){
      val lInput = new NEATInputProvider(neuralNetwork)
      val score = evaluate(lInput, rInput)
      println(s"Killed prototype. score = $score")
    }

  }

  def evaluate(lInput : PlayerInputProvider, rInput : PlayerInputProvider) = {
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
