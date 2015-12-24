package data

import neural.NeuralNetwork

class Species(var networks: Seq[NeuralNetwork]) {
  def getBestNetwork: NeuralNetwork = {
    networks.sortBy(x => x.score).head
  }
}
