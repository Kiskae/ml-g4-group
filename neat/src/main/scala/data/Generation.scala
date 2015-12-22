package data

import neural.NeuralNetwork

import scala.util.Random

/**
  * Created by bas on 6-12-15.
  */
class Generation(val species: Seq[Species]) {
  var currentGeneration = 0

  def networks: Seq[NeuralNetwork] = {
    species.flatMap(_.networks)
  }

  def evolve() = {

    mutate
    breed

    currentGeneration += 1
  }

  def mutate() = {
    val r = new Random

    for(n <- networks){
      val inputNeurons = n.getInputNeurons
      val startNeuron = inputNeurons(r.nextInt(n.getInputsCount))

      val outputNeurons = n.getOutputNeurons
      val endNeuron = outputNeurons(r.nextInt(n.getOutputsCount))
      n.createConnection(startNeuron, endNeuron, r.nextDouble()) //TODO what is the range of the weights?
    }
  }

  def breed() = {

  }
}
