package mutation

import neural.{Neuron, NeuralNetwork}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by bas on 22-12-15.
  */
object NetworkBreeder {
  def breed(network1: NeuralNetwork, network2: NeuralNetwork): NeuralNetwork = {
    val offspring = new NeuralNetwork()

    offspring.setInputNeurons(recreate(network1.getInputNeurons))
    offspring.setOutputNeurons(recreate(network1.getOutputNeurons))

    offspring.ensureHiddenNeurons(network2.getHiddenNeurons.map(_.label) ++ network1.getHiddenNeurons.map(_.label))

    val innovationNumbers = (network1.getInnovationNumbers ++ network2.getInnovationNumbers).distinct.sorted

    for(innovationNumber <- innovationNumbers){
      val connection1 = network1.getConnectionByNumber(innovationNumber)
      val connection2 = network2.getConnectionByNumber(innovationNumber)

      (connection1, connection2) match {
        case (Some(c1), Some(c2)) => {
          // Take the connection from the fittest parent
          if (network1.score > network2.score) {
            offspring.createConnection(offspring.getNeuron(c1._1.label), offspring.getNeuron(c1._2.label), c1._3)
          }else {
            offspring.createConnection(offspring.getNeuron(c2._1.label), offspring.getNeuron(c2._2.label), c2._3)
          }
        }
        case (Some(c1), None) => {
          offspring.createConnection(offspring.getNeuron(c1._1.label), offspring.getNeuron(c1._2.label), c1._3)
        }
        case (None, Some(c2)) => {
          offspring.createConnection(offspring.getNeuron(c2._1.label), offspring.getNeuron(c2._2.label), c2._3)
        }
      }
    }

    offspring
  }

  def recreate(neurons: Seq[Neuron]): Seq[Neuron] = {
    var newNeurons = new ArrayBuffer[Neuron]

    for(neuron <- neurons){
      val newNeuron = new Neuron(neuron.label)
      newNeurons += newNeuron
    }

    newNeurons
  }
}
