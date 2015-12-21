package neural

import scala.collection.mutable.ArrayBuffer

/**
  * Created by bas on 21-12-15.
  */
class NeuralNetwork {
  var numberOfNeurons = 0
  var neurons = ArrayBuffer[Neuron]
  var inputNeurons = ArrayBuffer[Neuron]
  var outputNeurons = ArrayBuffer[Neuron]

  def this(neuronsInCount: Int, neuronsOutCount: Int) {
    this
    ???
  }

  def createConnection(startNeuron: Neuron, endNeuron: Neuron, weight: Double) = ???
  def setInputNeurons(inputNeurons: Seq[Neuron]) = ???
  def setOutputNeurons(outputNeurons: Seq[Neuron]) = ???

  def setInput(inputs: Double*) = ???
  def calculate = ???
  def getOutput: Seq[Double] = ???

  def getInputsCount: Int = ???
  def getOutputsCount: Int = ???
  def getInputNeurons: Seq[Neuron] = ???
  def getOutputNeurons: Seq[Neuron] = ???
  def getWeights: Seq[Double] = ???

}
