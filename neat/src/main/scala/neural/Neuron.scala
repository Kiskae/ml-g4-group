package neural

import java.util.concurrent.atomic.AtomicInteger

/**
  * Created by bas on 21-12-15.
  */
@SerialVersionUID(3987547790973292510L)
class Neuron private(val label: Int, val layer: Int) extends Serializable {
  private var inputNeurons: Seq[InputNeuron] = Seq()
  /** value is used for the input values. */
  private var value: Option[Double] = None
  private var cachedValue: Option[Double] = None
  private var calculating: Boolean = false

  /**
    * Return the value if set, otherwise evaluate this
    * Neuron with the input neurons.
    */
  def evaluate(): Double = {
    value.orElse(cachedValue).getOrElse {
      assert(!calculating, s"evaluate loop on neuron ${System.identityHashCode(Neuron.this)} on thread ${Thread.currentThread()}")
      calculating = true
      try {
        val newVal = inputNeurons.map(n => n.other.evaluate() * n.weight).sum
        cachedValue = Some(newVal)
        newVal
      } finally {
        calculating = false
      }
    }
  }

  def resetCache() = {
    cachedValue = None
    calculating = false
  }

  def addInputNeuron(inputNeuron: Neuron, weight: Double, innovationNumber: Int) = {
    require(inputNeuron.layer < layer, s"$inputNeuron >= $this")
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

  override def toString = s"Neuron($label,in=$getInputLabels,layer=$layer)"

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

  def copy(): Neuron = new Neuron(label = this.label, layer = this.layer)
}

object Neuron {
  private val nodeSequence: AtomicInteger = new AtomicInteger(1)

  private def calcLayer(layerA: Int, layerB: Int): Int = {
    layerA / 2 + layerB / 2 //Good enough for now
  }

  def between(start: Neuron, end: Neuron): Neuron = new Neuron(
    nodeSequence.getAndIncrement(),
    calcLayer(start.layer, end.layer)
  )

  def staticSequence(layer: Int, len: Int, offset: Int = 0): Seq[Neuron] = {
    (0 until len).map(i => new Neuron(-i - offset, layer)).toSeq
  }
}
