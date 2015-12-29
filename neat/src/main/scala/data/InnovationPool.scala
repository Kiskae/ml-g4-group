package data

import neural.Neuron

import scala.collection.mutable

/**
  * Created by bas on 22-12-15.
  */
object InnovationPool {
  private val pool: mutable.Map[NeuronPair, Int] = mutable.Map()
  private var nextInnovationNumber = 0

  def getInnovationNumber(neuron1: Neuron, neuron2: Neuron): Int = {
    pool.getOrElseUpdate(NeuronPair(neuron1.label, neuron2.label), generateNextInnovationNumber)
  }

  private def generateNextInnovationNumber = {
    val tmp = nextInnovationNumber
    nextInnovationNumber += 1
    tmp
  }

  private case class NeuronPair(label1: Int, label2: Int)

}
