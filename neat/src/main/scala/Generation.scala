import org.neuroph.core.{Neuron, NeuralNetwork}
import org.neuroph.core.learning.LearningRule

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by bas on 6-12-15.
  */
class Generation(speciesCount : Int, networksPerSpecies: Int, inputLayerCount: Int, outputLayerCount: Int) {
  var species = new ArrayBuffer[Species]
  var currentGeneration = 0

  def initialize() = {
    for(i <- 0 until speciesCount){
      var networks = new ArrayBuffer[ENeuralNetwork[LearningRule]]

      for(i <- 0 until networksPerSpecies){
        val network = new ENeuralNetwork[LearningRule]

        network.makeInputLayer(inputLayerCount)
        network.makeOutputLayer(outputLayerCount)

        networks += (network)
      }

      species += (new Species(networks))
    }
  }

  def getAllNetworks() = {
    //TODO return all networks
    species(0).networks
  }

  def evolve() = {

    mutate
    breed

    currentGeneration += 1
  }

  def mutate() = {

  }

  def breed() = {

  }
}
