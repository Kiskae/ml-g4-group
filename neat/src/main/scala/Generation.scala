import org.neuroph.core.{Neuron, NeuralNetwork}
import org.neuroph.core.learning.LearningRule

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by bas on 6-12-15.
  */
class Generation(speciesCount : Int, networksPerSpecies: Int, inputLayerCount: Int, outputLayerCount: Int) {
  var species = new ArrayBuffer[Species]

  def initialize() = {
    for(i <- 0 until speciesCount){
//      var networks = ArrayBuffer[NeuralNetwork[LearningRule]]
//      val networks = List.fill(speciesCount)(new NeuralNetwork[LearningRule])

      var networks = new ArrayBuffer[NeuralNetwork[LearningRule]]

      for(i <- 0 until networksPerSpecies){
        val network = new NeuralNetwork[LearningRule]
        val inputNeurons = new ArrayBuffer[Neuron]
        val outputNeurons = new ArrayBuffer[Neuron]

        for(j <- 0 until inputLayerCount)
          inputNeurons += (new Neuron)

        for(k <- 0 until outputLayerCount)
          outputNeurons += (new Neuron)

        networks += (network)
      }

      species += (new Species(networks))
    }
  }

  def getAllNetworks() = {
    //TODO return all networks
    species(0).networks
  }
}
