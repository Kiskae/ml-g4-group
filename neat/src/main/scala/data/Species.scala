package data

import neural.NeuralNetwork

class Species(var networks: Seq[NeuralNetwork]) extends Serializable {
  def getBestNetwork: NeuralNetwork = {
    networks.sortBy(x => x.score).last
  }
}
