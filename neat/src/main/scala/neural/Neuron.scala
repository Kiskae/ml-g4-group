package neural

import scala.collection.mutable.ArrayBuffer

/**
  * Created by bas on 21-12-15.
  */
class Neuron(val label: Int) {
  var inputNeurons = new ArrayBuffer[Tuple2[Neuron, Double]]

  def evaluate: Double = ???
  def addInputNeuron = ???

  def getWeights: Seq[Double] = inputNeurons.map(_._2)
}
