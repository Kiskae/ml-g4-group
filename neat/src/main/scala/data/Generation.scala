package data

import mutation.NetworkBreeder
import neural.NeuralNetwork

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by bas on 6-12-15.
  */
class Generation(var species: Seq[Species]) {
  var currentGeneration = 0
  val eliminationPercentage = 0.9
  val newConnectionProbability = 0.2

  def networks: Seq[NeuralNetwork] = {
    species.flatMap(_.networks)
  }

  def evolve() = {


    mutate
    breed

    currentGeneration += 1
  }

  def mutate() = {
    val r = new Random

    for(n <- networks){
      if(r.nextInt((1 / newConnectionProbability).toInt) == 0) {
        val inputNeurons = n.getInputNeurons
        val startNeuron = inputNeurons(r.nextInt(n.getInputsCount))

        val outputNeurons = n.getOutputNeurons
        val endNeuron = outputNeurons(r.nextInt(n.getOutputsCount))
        n.createConnection(startNeuron, endNeuron, r.nextDouble() * (r.nextInt(9) - 4)) //TODO what is the range of the weights?
      }
    }
  }

  def breed(): Any = {
    var newSpecies = new ArrayBuffer[Species]

    for(specie <- species){
      //For each specie: murder the bottom N percent, replace population
      //by breeding the rest.

      val networks = specie.networks
      networks.sortBy(_.score)

      var offspring = new ArrayBuffer[NeuralNetwork]

      //create distribution map
      var distribution = mutable.Map[NeuralNetwork, Double]()
      val fitnessMin = networks.map(_.score) min
      val fitnessMax = networks.map(_.score) max

      networks.foreach(x => x.score = x.score + Math.abs(fitnessMin) )
      val fitnessSum = networks.map(_.score) sum

      // TODO what to do if the fitness sum is zero? all prototypes are bad, kill off species?
      //TODO what about negative fitness? Will mess up the distribution
      if(fitnessSum == 0) return

      for(network <- networks){
        distribution += network -> (network.score / fitnessSum.toFloat)
      }

      // sample
      for(i <- 0 until specie.networks.length){
        val network1 = sample[NeuralNetwork](distribution)
        val network2 = sample[NeuralNetwork](distribution)

        //TODO Should we make sure they're not the same network?
        offspring += NetworkBreeder.breed(network1, network2)
      }

      newSpecies += new Species(offspring)
    }

    species = newSpecies
  }

  // Adapted from http://stackoverflow.com/a/24869852/1357218
  final def sample[A](dist: mutable.Map[A, Double]): A = {
    val p = scala.util.Random.nextDouble
    val it = dist.iterator
    var accum = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      accum += itemProb
      if (accum >= p)
        return item  // return so that we don't have to search through the whole distribution
    }
    sys.error(f"this should never happen")  // needed so it will compile
  }

  def getBestPrototypes: Seq[NeuralNetwork] = {
    species.map(_.getBestNetwork)
  }
}
