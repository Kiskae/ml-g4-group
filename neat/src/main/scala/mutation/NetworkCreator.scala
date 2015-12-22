package mutation

import data.{ENeuralNetwork, Generation, Species}
import neural.NeuralNetwork
import org.neuroph.core.learning.LearningRule

object NetworkCreator {
  def generation(speciesCount: Int, networksPerSpecies: Int, inputLayerCount: Int, outputLayerCount: Int): Generation = {
    new Generation(Seq.fill(speciesCount) {
      createSpecies(networksPerSpecies) {
        createNetwork(inputLayerCount, outputLayerCount)
      }
    })
  }

  private def createSpecies(networksPerSpecies: Int)(createNetwork: => NeuralNetwork): Species = {
    new Species(Seq.fill[NeuralNetwork](networksPerSpecies)(createNetwork))
  }

  private def createNetwork(inputLayerCount: Int, outputLayerCount: Int): NeuralNetwork = {
    val network = new NeuralNetwork(inputLayerCount, outputLayerCount)
    network
  }
}
