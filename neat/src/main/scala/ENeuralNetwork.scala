import org.neuroph.core.NeuralNetwork
import org.neuroph.core.learning.LearningRule

/**
  * Created by bas on 10-12-15.
  */
class ENeuralNetwork[L <: LearningRule] extends NeuralNetwork{
  var score = 0
}
