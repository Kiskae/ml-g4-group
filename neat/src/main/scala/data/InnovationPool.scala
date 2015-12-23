package data

import neural.Neuron

import scala.collection.mutable.Map

/**
  * Created by bas on 22-12-15.
  */
object InnovationPool {
  var pool: Map[(Neuron, Neuron), Int] = Map[(Neuron, Neuron), Int]()
  var nextInnovationNumber = 0

  def getInnovationNumber(neurons : (Neuron, Neuron)): Int = {
    val innovationNumber = pool.get(neurons)
println("innovation number = " + innovationNumber)
    if(innovationNumber != None){
      innovationNumber.get
    }else{
      pool.put(neurons, nextInnovationNumber)
      val temp = nextInnovationNumber
      nextInnovationNumber += 1
      temp
    }
  }
}
