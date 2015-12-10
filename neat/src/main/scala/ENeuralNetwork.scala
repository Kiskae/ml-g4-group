import grizzled.slf4j.Logging
import org.neuroph.core.{Neuron, Layer, NeuralNetwork}
import org.neuroph.core.learning.LearningRule

import scala.util.Random

/**
  * Created by bas on 10-12-15.
  */
class ENeuralNetwork[L <: LearningRule] extends NeuralNetwork with Logging{
  var score = 0

  def mutate() = {
    val rand = new Random

    // Add neuron with probability 0.1
    if(rand.nextInt(10) == 0){

    }

  }
}
