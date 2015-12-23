package data

import neural.Neuron

import scala.collection.mutable.Map

/**
  * Created by bas on 22-12-15.
  */
object InnovationPool {
  var pool: Map[(Int, Int), Int] = Map[(Int, Int), Int]()
  var nextInnovationNumber = 0

  def getInnovationNumber(neurons : (Neuron, Neuron)): Int = {
    val innovationNumber = pool.get((neurons._1.label, neurons._2.label))

    if(innovationNumber != None){
      innovationNumber.get
    }else{
      pool.put((neurons._1.label, neurons._2.label), nextInnovationNumber)
      val temp = nextInnovationNumber
      nextInnovationNumber += 1
      temp
    }
  }
}
