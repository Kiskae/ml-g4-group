package data

import grizzled.slf4j.Logging
import org.neuroph.core.learning.LearningRule
import org.neuroph.core.{NeuralNetwork, Neuron}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by bas on 10-12-15.
  */
class ENeuralNetwork[L <: LearningRule] extends NeuralNetwork[L] with Logging {
  var score = 0

  def mutate() = {
    val rand = new Random

    // Add neuron with probability 0.1
    if (rand.nextInt(10) == 0) {

    }

    //Remove neuron ?

    //Add connection
    //Get innovation number
    //this.createConnection()


    //remove connection

    //change connection weight

  }

  //TODO: add innovation numbers support
  def makeInputLayer(inputLayerCount: Int) = {
    val inputNeurons = new ArrayBuffer[Neuron]

    for (j <- 0 until inputLayerCount) {
      inputNeurons += new Neuron
    }

    this.setInputNeurons(inputNeurons.toArray)
  }

  //TODO: add innovation numbers support
  def makeOutputLayer(outputLayerCount: Int) = {
    val outputNeurons = new ArrayBuffer[Neuron]

    for (k <- 0 until outputLayerCount) {
      outputNeurons += new Neuron
    }

    this.setOutputNeurons(outputNeurons.toArray)
  }

}
