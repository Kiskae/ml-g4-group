package data

/**
  * Created by bas on 6-12-15.
  */
class Generation(val species: Seq[Species]) {
  var currentGeneration = 0

  def networks: Seq[ENeuralNetwork[_]] = {
    species.flatMap(_.networks)
  }

  def evolve() = {

    mutate
    breed

    currentGeneration += 1
  }

  def mutate() = {

  }

  def breed() = {

  }
}
