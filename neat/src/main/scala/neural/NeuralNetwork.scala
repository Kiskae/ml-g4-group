package neural

import scala.collection.mutable.ArrayBuffer

/**
  * Created by bas on 21-12-15.
  */
class NeuralNetwork {
  var numberOfNeurons = 0
  var hiddenNeurons = new ArrayBuffer[Neuron]
  var inputNeurons = new ArrayBuffer[Neuron]
  var outputNeurons = new ArrayBuffer[Neuron]
  var output = null

  def this(neuronsInCount: Int, neuronsOutCount: Int) {
    this
    ???
  }

  def neurons: Seq[Neuron] = hiddenNeurons ++ inputNeurons ++ outputNeurons

  def createConnection(startNeuron: Neuron, endNeuron: Neuron, weight: Double) = ???

  def setInputNeurons(inputNeurons: Seq[Neuron]) = ???

  def setOutputNeurons(outputNeurons: Seq[Neuron]) = ???

  def setInput(inputs: Double*) = ???

  def evaluate = ???

  def getOutput: Seq[Double] = output

  def getInputsCount: Int = inputNeurons.length

  def getOutputsCount: Int = outputNeurons.length

  def getInputNeurons: Seq[Neuron] = inputNeurons

  def getOutputNeurons: Seq[Neuron] = outputNeurons

  def getWeights: Seq[Double] = ???

}
