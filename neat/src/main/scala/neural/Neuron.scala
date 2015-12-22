package neural

import scala.collection.mutable.ArrayBuffer

/**
  * Created by bas on 21-12-15.
  */
class Neuron(val label: Int) extends Serializable{
  var inputNeurons = new ArrayBuffer[(Neuron, Double, Int)]
  /** value is used for the input values. */
  var value = 0.0
  var valueIsSet = false

  /**
    * Return the value if set, otherwise evaluate this
    * Neuron with the input neurons.
    */
  def evaluate: Double = {
    if(valueIsSet){
      value
    }else{
      val as = inputNeurons.map(_._1.evaluate)
      val bs = inputNeurons.map(_._2)
      (for ((a, b) <- as zip bs) yield a * b) sum
    }
  }

  def addInputNeuron(inputNeuron: Neuron, weight: Double, innovationNumber: Int) = {
    inputNeurons += Tuple3(inputNeuron, weight, innovationNumber)
  }

  def getWeights: Seq[Double] = inputNeurons.map(_._2)

  def setValue(value: Double) = {
    this.value = value
    this.valueIsSet = true
  }
}
