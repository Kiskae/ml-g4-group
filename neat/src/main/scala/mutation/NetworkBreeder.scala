package mutation

import neural.Neuron
import neural.{Neuron, NeuralNetwork}
import org.neuroph.core.Neuron

/**
  * Created by bas on 22-12-15.
  */
object NetworkBreeder {
  def breed(network1: NeuralNetwork, network2: NeuralNetwork): NeuralNetwork = {
    val offspring = new NeuralNetwork()

    offspring.setInputNeurons(network1.getInputNeurons) // TODO Is it OK to use references here?
    offspring.setOutputNeurons(network1.getOutputNeurons)

    val innovationNumbers = (network1.getInnovationNumbers ++ network2.getInnovationNumbers).distinct.sorted

    for(innovationNumber <- innovationNumbers){
      val connection1 = network1.getConnectionByNumber(innovationNumber)
      val connection2 = network2.getConnectionByNumber(innovationNumber)

      (connection1, connection2) match {
        case (Some(c1), Some(c2)) => {
          // Take the connection from the fittest parent
          if (network1.score > network2.score) {
            offspring.createConnection(c1._1, c1._2, c1._3)
          }else {
            offspring.createConnection(c2._1, c2._2, c2._3)
          }
        }
        case (Some(c1), None) => {
          offspring.createConnection(c1._1, c1._2, c1._3)
        }
        case (None, Some(c2)) => {
          offspring.createConnection(c2._1, c2._2, c2._3)
        }
      }
    }

    offspring
  }
}
