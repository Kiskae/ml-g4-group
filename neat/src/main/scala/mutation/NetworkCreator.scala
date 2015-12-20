package mutation

import data.{ENeuralNetwork, Generation, Species}
import org.neuroph.core.learning.LearningRule

object NetworkCreator {
  def generation(speciesCount: Int, networksPerSpecies: Int, inputLayerCount: Int, outputLayerCount: Int): Generation = {
    new Generation(Seq.fill(speciesCount) {
      createSpecies(networksPerSpecies) {
        createNetwork(inputLayerCount, outputLayerCount)
      }
    })
  }

  private def createSpecies(networksPerSpecies: Int)(createNetwork: => ENeuralNetwork[_]): Species = {
    new Species(Seq.fill[ENeuralNetwork[_]](networksPerSpecies)(createNetwork))
  }

  private def createNetwork(inputLayerCount: Int, outputLayerCount: Int): ENeuralNetwork[_] = {
    val network = new ENeuralNetwork[LearningRule]
    network.makeInputLayer(inputLayerCount)
    network.makeOutputLayer(outputLayerCount)
    network
  }
}
