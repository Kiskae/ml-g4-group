package neural

import data.InnovationPool

import scala.collection.mutable.ArrayBuffer

/**
  * Created by bas on 21-12-15.
  */
class NeuralNetwork(neuronsInCount: Int = 0, neuronsOutCount: Int = 0) extends Serializable {
  var numberOfNeurons = 0
  var hiddenNeurons = new ArrayBuffer[Neuron]
  var inputNeurons = ArrayBuffer.fill[Neuron](neuronsInCount)(newNeuron)
  var outputNeurons = ArrayBuffer.fill[Neuron](neuronsOutCount)(newNeuron)
  var output: Seq[Double] = null
  var score = 0

  def newNeuron: Neuron = new Neuron(getNextLabelAndIncrement)

  private def getNextLabelAndIncrement = {
    val t = numberOfNeurons
    numberOfNeurons += 1
    t
  }

  def createConnection(startNeuron: Neuron, endNeuron: Neuron, weight: Double) = {

    val innovationNumber = InnovationPool.getInnovationNumber((startNeuron, endNeuron))
    endNeuron.addInputNeuron(startNeuron, weight, innovationNumber)
  }

  def setInput(inputs: Double*) = {
    if (inputs.length != inputNeurons.length) {
      throw new IllegalArgumentException("Size of input values doesn't match size of input neurons.")
    }

    (neurons, inputs).zipped.foreach((neuron, input) => neuron.setValue(input))
  }

  def evaluate: Seq[Double] = {
    var outputs = new ArrayBuffer[Double]

    outputNeurons.foreach(n => outputs += n.evaluate)

    output = outputs
    outputs
  }

  def getOutput: Seq[Double] = output

  def getInputsCount: Int = inputNeurons.length

  def getOutputsCount: Int = outputNeurons.length

  def getInputNeurons: Seq[Neuron] = inputNeurons

  def setInputNeurons(inputNeurons: Seq[Neuron]) = {
    this.inputNeurons = inputNeurons.to[ArrayBuffer]
  }

  def getOutputNeurons: Seq[Neuron] = outputNeurons

  def setOutputNeurons(outputNeurons: Seq[Neuron]) = {
    this.outputNeurons = outputNeurons.to[ArrayBuffer]
  }

  def getWeights: Seq[Double] = neurons.flatMap(_.getWeights)

  def neurons: Seq[Neuron] = hiddenNeurons ++ inputNeurons ++ outputNeurons

  def addHiddenNeuron(neuron: Neuron) = hiddenNeurons += neuron
}

object NeuralNetworkUtil {
  def threshold(value: Double): Int = if (value < 0) 0 else 1
}