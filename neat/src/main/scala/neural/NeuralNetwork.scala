package neural

import data.InnovationPool

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class NeuralNetwork(neuronsInCount: Int = 0, neuronsOutCount: Int = 0) extends Serializable {
  var numberOfNeurons = 0
  var hiddenNeurons = new ArrayBuffer[Neuron]
  var inputNeurons = makeInputNeurons(neuronsInCount)
  var outputNeurons = makeOutputNeurons(neuronsOutCount, neuronsInCount)
  private var connections = new ArrayBuffer[(Neuron, Neuron, Double, Int)]
  var output: Seq[Double] = null
  var score = 0.0

  def newNeuron: Neuron = new Neuron(getNextLabelAndIncrement)

  def makeInputNeurons(neuronsInCount: Int): Seq[Neuron] = {
    var inputNeurons = new ArrayBuffer[Neuron]

    for(i <- 0 until neuronsInCount){
      inputNeurons += new Neuron(i)
      numberOfNeurons += 1
    }

    inputNeurons
  }

  def makeOutputNeurons(neuronsOutCount: Int, neuronsInCount: Int): Seq[Neuron] = {
    var outputNeurons = new ArrayBuffer[Neuron]

    for(i <- 0 until neuronsOutCount){
      outputNeurons += new Neuron(i + neuronsInCount)
      numberOfNeurons += 1
    }

    outputNeurons
  }

  private def getNextLabelAndIncrement = {
    val t = numberOfNeurons
    numberOfNeurons += 1
    t
  }

  /**
    * If the neurons don't yet exist in the network, add them.
    * Assumes that the input and output neurons exist.
    */
  def createConnection(startNeuron: Neuron, endNeuron: Neuron, weight: Double) = {
    if(!inputNeurons.map(_.label).contains(startNeuron.label) && !hiddenNeurons.map(_.label).contains(startNeuron.label)){
      hiddenNeurons += startNeuron
      numberOfNeurons += 1
    }

    if(!outputNeurons.map(_.label).contains(endNeuron.label) && !hiddenNeurons.map(_.label).contains(endNeuron.label)){
      hiddenNeurons += endNeuron
      numberOfNeurons += 1
    }

    val innovationNumber = InnovationPool.getInnovationNumber((startNeuron, endNeuron))

    if(!endNeuron.getInputLabels.contains(startNeuron.label)){
      endNeuron.addInputNeuron(startNeuron, weight, innovationNumber)
      connections += Tuple4(startNeuron, endNeuron, weight, innovationNumber)
    }
  }

  def assertConsistent = {

    for(x <- connections){
      if(!x._2.inputNeurons.map(_._1.label).contains(x._1.label)) {
        println(connections.length)
        throw new Exception
      }
    }
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
    numberOfNeurons += inputNeurons.length
  }

  def getOutputNeurons: Seq[Neuron] = outputNeurons

  def setOutputNeurons(outputNeurons: Seq[Neuron]) = {
    this.outputNeurons = outputNeurons.to[ArrayBuffer]
    numberOfNeurons += outputNeurons.length
  }

  def getWeights: Seq[Double] = connections.map(_._3)

  def neurons: Seq[Neuron] = hiddenNeurons ++ inputNeurons ++ outputNeurons

  def addHiddenNeuron(neuron: Neuron) = {
    hiddenNeurons += neuron
  }

  def getInnovationNumbers: Seq[Int] = {
    connections.map(_._4)
  }

  def getConnectionByNumber(innovationNumber: Int): Option[(Neuron, Neuron, Double, Int)] = {
    val innovationNumbers = getInnovationNumbers

    if(innovationNumbers.contains(innovationNumber)){
      Some(connections(innovationNumbers.indexOf(innovationNumber)))
    }else{
      None
    }
  }

  def getConnections = connections

  def getNeuron(label: Int): Neuron = {
    try{
      neurons(neurons.map(_.label).indexOf(label))
    }catch{
      case ex: ArrayIndexOutOfBoundsException => {
        println("out o fobuntdsa")
        throw ex
      }
    }
  }

  def getHiddenNeurons: Seq[Neuron] = hiddenNeurons

  def ensureHiddenNeurons(labels: Seq[Int]) = {
    val toAdd = labels.toSet -- hiddenNeurons.map(_.label)
    toAdd.foreach(label => this.addHiddenNeuron(new Neuron(label)))
  }

  def deleteRandomConnection: Any = {
    val r = new Random

    if(connections.length == 0) return

    val index = r.nextInt(connections.length)
    val connection = connections(index)

    connection._2.removeInputNeuron(connection._1.label)
    connections.remove(index)
  }

  override def toString = s"NeuralNetwork($score)"
}

object NeuralNetworkUtil {
  def threshold(value: Double): Int = if (value < 0) 0 else 1
}