package neural

import java.util.concurrent.locks.ReentrantLock

import data.InnovationPool

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

@SerialVersionUID(3096133326619037005L)
class NeuralNetwork(neuronsInCount: Int = 0, neuronsOutCount: Int = 0) extends Serializable {
  private var hiddenNeurons = new ArrayBuffer[Neuron]
  private var inputNeurons = Neuron.staticSequence(0, neuronsInCount)
  private var outputNeurons = Neuron.staticSequence(Int.MaxValue, neuronsOutCount, neuronsInCount)
  private var connections: IndexedSeq[NeuralNetwork.Connection] = IndexedSeq()
  private var output: Option[Seq[Double]] = None
  var score = 0.0
  private val evalLock = new ReentrantLock()

  /**
    * If the neurons don't yet exist in the network, add them.
    * Assumes that the input and output neurons exist.
    */
  def createConnection(startNeuron: Neuron, endNeuron: Neuron, weight: Double) = {
    require(startNeuron != endNeuron)
    if (!(inputNeurons.contains(startNeuron) || hiddenNeurons.contains(startNeuron))) {
      addHiddenNeuron(startNeuron)
    }

    if (!(outputNeurons.contains(endNeuron) || hiddenNeurons.contains(endNeuron))) {
      addHiddenNeuron(endNeuron)
    }

    val innovationNumber = InnovationPool.getInnovationNumber(startNeuron, endNeuron)

    if (!endNeuron.getInputNeurons.contains(startNeuron)) {
      endNeuron.addInputNeuron(startNeuron, weight, innovationNumber)
      connections = connections :+ NeuralNetwork.Connection(startNeuron, endNeuron, weight, innovationNumber)
    }
  }

  def assertConsistent = {
    connections.foreach { conn =>
      if (!conn.end.getInputNeurons.contains(conn.start)) {
        throw new IllegalStateException(s"Missing connection: $conn")
      }
    }
  }

  private def setInput(inputs: Double*) = {
    if (inputs.length != inputNeurons.length) {
      throw new IllegalArgumentException("Size of input values doesn't match size of input neurons.")
    }

    (neurons, inputs).zipped.foreach((neuron, input) => neuron.setValue(input))
  }

  def evaluate(inputs: Double*): Seq[Double] = {
    assert(evalLock.tryLock(), s"Evaluate collision on ${Thread.currentThread()}")
    try {
      setInput(inputs: _*)
      neurons.foreach(_.resetCache())
      output = Some(outputNeurons.map(_.evaluate()))
      output.get
    } finally {
      evalLock.unlock()
    }
  }

  def thresholdOutput: Seq[Int] = {
    getOutput.map(out => if (out < 0) 0 else 1)
  }

  def getOutput: Seq[Double] = output.get

  def getInputsCount: Int = inputNeurons.length

  def getOutputsCount: Int = outputNeurons.length

  def getInputNeurons: Seq[Neuron] = inputNeurons

  def setInputNeurons(inputNeurons: Seq[Neuron]) = {
    this.inputNeurons = inputNeurons
  }

  def getOutputNeurons: Seq[Neuron] = outputNeurons

  def setOutputNeurons(outputNeurons: Seq[Neuron]) = {
    this.outputNeurons = outputNeurons
  }

  def getWeights: Seq[Double] = connections.map(_.weight)

  def neurons: Seq[Neuron] = hiddenNeurons ++ inputNeurons ++ outputNeurons

  def addHiddenNeuron(neuron: Neuron) = {
    hiddenNeurons += neuron
  }

  def getInnovationNumbers: Seq[Int] = {
    connections.map(_.innovation)
  }

  def getConnectionByNumber(innovationNumber: Int): Option[NeuralNetwork.Connection] = {
    connections.find(_.innovation == innovationNumber)
  }

  def getConnections = connections

  def getNeuron(label: Int): Neuron = {
    neurons.find(_.label == label).get
  }

  def getHiddenNeurons: Seq[Neuron] = hiddenNeurons

  def ensureHiddenNeurons(neurons: Seq[Neuron]) = {
    val toAdd: Set[Neuron] = neurons.toSet -- hiddenNeurons
    toAdd.foreach(n => this.addHiddenNeuron(n.copy()))
  }

  def deleteRandomConnection(rand: Random): Any = {
    if (connections.isEmpty) return

    val index = rand.nextInt(connections.length)
    val connection = connections(index)

    deleteConnection(connection)
  }

  def getRandomConnection(rand: Random) = {
    val index = rand.nextInt(connections.length)
    connections(index)
  }

  def deleteConnection(connection: NeuralNetwork.Connection) = {
    connection.end.removeInputNeuron(connection.start.label)
    connections = connections.filterNot(_ == connection)
  }

  override def toString = s"NeuralNetwork($score)"
}

object NeuralNetwork {

  case class Connection(start: Neuron, end: Neuron, var weight: Double, innovation: Int)

}