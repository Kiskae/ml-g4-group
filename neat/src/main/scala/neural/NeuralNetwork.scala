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
  var output: Seq[Double] = null

  def this(neuronsInCount: Int, neuronsOutCount: Int) {
    this
    inputNeurons = ArrayBuffer.fill[Neuron](neuronsInCount)(new Neuron(getNextLabelAndIncrement))
    outputNeurons = ArrayBuffer.fill[Neuron](neuronsOutCount)(new Neuron(getNextLabelAndIncrement))
  }

  def getNextLabelAndIncrement = {
    val t = numberOfNeurons
    numberOfNeurons += 1
    t
  }

  def neurons: Seq[Neuron] = hiddenNeurons ++ inputNeurons ++ outputNeurons

  def createConnection(startNeuron: Neuron, endNeuron: Neuron, weight: Double) = {
    endNeuron.addInputNeuron(startNeuron, weight)
  }

  def setInputNeurons(inputNeurons: Seq[Neuron]) = {
    this.inputNeurons = inputNeurons.to[ArrayBuffer]
  }

  def setOutputNeurons(outputNeurons: Seq[Neuron]) = {
    this.outputNeurons = outputNeurons.to[ArrayBuffer]
  }

  def setInput(inputs: Double*) = {
    if(inputs.length != inputNeurons.length){
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

  def getOutputNeurons: Seq[Neuron] = outputNeurons

  def getWeights: Seq[Double] = neurons.flatMap(_.getWeights)

  def addHiddenNeuron(neuron: Neuron) = hiddenNeurons += neuron
}

object NeuralNetworkUtil {
  def treshold(value: Double): Int = if(value < 0) 0 else 1
}