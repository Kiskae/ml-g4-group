package neural

/**
  * Created by bas on 21-12-15.
  */
class Neuron(val label: Int) extends Serializable {
  private var inputNeurons: Seq[InputNeuron] = Seq()
  /** value is used for the input values. */
  var value: Option[Double] = None

  /**
    * Return the value if set, otherwise evaluate this
    * Neuron with the input neurons.
    */
  def evaluate(depth: Int): Double = {
    if (depth > 20) {
      println("DERP")
    }
    value.getOrElse {
      inputNeurons.map(n => n.other.evaluate(depth + 1) * n.weight).sum
    }
  }

  def addInputNeuron(inputNeuron: Neuron, weight: Double, innovationNumber: Int) = {
    require(inputNeuron != this)
    inputNeurons = inputNeurons :+ InputNeuron(inputNeuron, weight, innovationNumber)
  }

  def getWeights: Seq[Double] = inputNeurons.map(_.weight)

  def setValue(value: Double) = {
    this.value = Some(value)
  }

  def getInputNeurons: Seq[Neuron] = {
    inputNeurons.map(_.other)
  }

  def getInputLabels: Seq[Int] = {
    getInputNeurons.map(_.label)
  }

  def removeInputNeuron(label: Int) = {
    inputNeurons = inputNeurons.filterNot(_.other.label == label)
  }

  override def toString = s"Neuron($label,in=$getInputLabels)"

  private case class InputNeuron(other: Neuron, weight: Double, innovation: Int)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Neuron]

  override def equals(other: Any): Boolean = other match {
    // Not really full implementation of equals, but good enough
    case that: Neuron =>
      (that canEqual this) &&
        label == that.label
    case _ => false
  }

  override def hashCode(): Int = {
    label.hashCode()
  }
}
