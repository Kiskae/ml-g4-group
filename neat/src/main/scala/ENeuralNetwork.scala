import grizzled.slf4j.Logging
import org.neuroph.core.{Neuron, Layer, NeuralNetwork}
import org.neuroph.core.learning.LearningRule

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by bas on 10-12-15.
  */
class ENeuralNetwork[L <: LearningRule] extends NeuralNetwork with Logging{
  var score = 0

  def mutate() = {
    val rand = new Random

    // Add neuron with probability 0.1
    if(rand.nextInt(10) == 0){

    }

  }

  //TODO: add innovation numbers support
  def makeInputLayer(inputLayerCount: Int) = {
    val inputNeurons = new ArrayBuffer[Neuron]

    for(j <- 0 until inputLayerCount) {
      inputNeurons += (new Neuron)
    }

    this.setInputNeurons(inputNeurons.toArray)
  }

  //TODO: add innovation numbers support
  def makeOutputLayer(outputLayerCount: Int) = {
    val outputNeurons = new ArrayBuffer[Neuron]

    for(k <- 0 until outputLayerCount) {
      outputNeurons += (new Neuron)
    }

    this.setOutputNeurons(outputNeurons.toArray)
  }

}
