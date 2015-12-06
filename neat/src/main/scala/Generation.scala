import org.neuroph.core.NeuralNetwork
import org.neuroph.core.learning.LearningRule

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by bas on 6-12-15.
  */
class Generation(speciesCount : Int, networksPerSpecies: Int) {
  var species = new ArrayBuffer[Species]

  def initialize() = {
    for(i <- 0 until speciesCount){
//      var networks = ArrayBuffer[NeuralNetwork[LearningRule]]
//      val networks = List.fill(speciesCount)(new NeuralNetwork[LearningRule])

      var networks = new ArrayBuffer[NeuralNetwork[LearningRule]]

      for(i <- 0 until networksPerSpecies){
        networks += (new NeuralNetwork[LearningRule])
      }

      species += (new Species(networks))
    }
  }
}
