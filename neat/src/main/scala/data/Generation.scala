package data

import java.util.concurrent.ThreadLocalRandom

import misc.Probability
import mutation.NetworkBreeder
import neural.NeuralNetwork

import scala.collection.mutable
import scala.util.Random

class Generation(var species: Seq[Species]) extends Serializable {
  var currentGeneration = 0

  def networks: Seq[NeuralNetwork] = {
    species.flatMap(_.networks)
  }

  def evolve() = {
    val r = ThreadLocalRandom.current()

//    mutate(r)
//    breed(r)

    currentGeneration += 1
  }

  def mutate(r: Random) = {
    for (n <- networks) {
      if (Generation.newConnectionProbability.test(r)) {
        val inputNeurons = n.getInputNeurons ++ n.hiddenNeurons
        var startNeuron = inputNeurons(r.nextInt(inputNeurons.length))

        val outputNeurons = n.getOutputNeurons ++ n.hiddenNeurons
        var endNeuron = outputNeurons(r.nextInt(outputNeurons.length))

        while (startNeuron.label <= endNeuron.label && !n.getOutputNeurons.contains(endNeuron)) {
          startNeuron = inputNeurons(r.nextInt(inputNeurons.length))
          endNeuron = outputNeurons(r.nextInt(outputNeurons.length))
        }

        n.createConnection(startNeuron, endNeuron, newConnectionWeight(r))
      }

      if (Generation.newNodeProbability.test(r) && n.getConnections.length > 0) {
        val connection = n.getRandomConnection(r)
        val startNode = connection.start
        val endNode = connection.end
        val weight = connection.weight

        n.deleteConnection(connection)

        val newNeuron = n.newNeuron
        n.addHiddenNeuron(newNeuron)

        n.createConnection(startNode, newNeuron, weight)
        n.createConnection(newNeuron, endNode, newConnectionWeight(r))
      }

      if (Generation.changeWeightProbability.test(r) && n.getConnections.length > 0) {
        val connections = n.getConnections

        val c = connections(r.nextInt(connections.length))
        c.weight = newConnectionWeight(r)
      }

      if (Generation.deleteConnectionProbability.test(r)) {
        n.deleteRandomConnection(r)
      }
    }
  }

  private def newConnectionWeight(r: Random): Double = {
    r.nextDouble() * (r.nextInt(9) - 4) //TODO what is the range of the weights?
  }

  def breed(r: Random) = {
    species = species.map { specie =>
      //For each specie: murder the bottom N percent, replace population
      //by breeding the rest.
      val networks = specie.networks
      (specie, networks.sortBy(_.score).slice((networks.length * Generation.eliminationPercentage).toInt, networks.length))
    }.map { case (specie, networks) =>
      val scores = networks.map(_.score)
      val min = math.abs(scores.min)
      val sum = scores.map(_ + min).sum

      if (sum != 0) {
        //create distribution map
        val distribution = mutable.Map[NeuralNetwork, Double]()
        networks.foreach { network =>
          distribution.put(network, (network.score + min) / sum.toDouble)
        }

        //generate as many networks as there originally were
        new Species(Seq.fill(specie.networks.size) {
          NetworkBreeder.breed(
            sample(distribution, r),
            sample(distribution, r)
          )
        })
      } else {
        specie
      }
    }
  }

  // Adapted from http://stackoverflow.com/a/24869852/1357218
  final def sample[A](dist: mutable.Map[A, Double], r: Random): A = {
    val p = r.nextDouble()
    val it = dist.iterator
    var accum = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      accum += itemProb
      if (accum >= p)
        return item // return so that we don't have to search through the whole distribution
    }
    sys.error(f"this should never happen") // needed so it will compile
  }

  def getBestPrototypes: Seq[NeuralNetwork] = {
    species.map(_.getBestNetwork)
  }
}

object Generation {
  val eliminationPercentage = 0.3
  val newConnectionProbability = Probability(0.1)
  val newNodeProbability = Probability(1.0)
  val deleteConnectionProbability = Probability(0.05)
  val changeWeightProbability = Probability(0.15)
}
