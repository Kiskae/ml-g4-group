package mutation

import neural.{NeuralNetwork, Neuron}

/**
  * Created by bas on 22-12-15.
  */
object NetworkBreeder {
  def breed(network1: NeuralNetwork, network2: NeuralNetwork): NeuralNetwork = {
    val offspring = new NeuralNetwork()

    offspring.setInputNeurons(recreate(0, network1.getInputNeurons))
    offspring.setOutputNeurons(recreate(Int.MaxValue, network1.getOutputNeurons))

    offspring.ensureHiddenNeurons(network2.getHiddenNeurons ++ network1.getHiddenNeurons)

    val innovationNumbers = (network1.getInnovationNumbers ++ network2.getInnovationNumbers).distinct.sorted

    for (innovationNumber <- innovationNumbers) {
      val connection1 = network1.getConnectionByNumber(innovationNumber)
      val connection2 = network2.getConnectionByNumber(innovationNumber)

      (connection1, connection2) match {
        case (Some(c1), Some(c2)) =>
          // Take the connection from the fittest parent
          if (network1.score > network2.score) {
            buildConnection(offspring, c1)
          } else {
            buildConnection(offspring, c2)
          }
        case (Some(c1), None) => buildConnection(offspring, c1)
        case (None, Some(c2)) => buildConnection(offspring, c2)
        case (None, None) => throw new AssertionError(s"Innovation: $innovationNumber")
      }
    }

    offspring
  }

  private def buildConnection(out: NeuralNetwork, template: NeuralNetwork.Connection) = {
    out.createConnection(out.getNeuron(template.start.label), out.getNeuron(template.end.label), template.weight)
  }

  def recreate(layer: Int, neurons: Seq[Neuron]): Seq[Neuron] = {
    neurons.map(n => n.copy())
  }
}
