import agent.{BallFollower, PlayerInputProvider}
import grizzled.slf4j.Logging
import mutation.NetworkCreator
import server.{GameProperties, GameState, PhysicsProperties}

/**
  * Created by bas on 5-12-15.
  */
object NEAT extends Logging {

  val maxIdleSteps = 5000

  def main(args: Array[String]) = {
    logger.info("Starting NEAT!")
    //    val lInput = new BallFollower(30000L / 2)
    val rInput = new BallFollower(30000L / 2)

    val speciesCount = 10
    val networksPerSpecies = 100
    val inputLayerCount = 4
    val outputLayerCount = 3
    val generation = NetworkCreator.generation(speciesCount, networksPerSpecies, inputLayerCount, outputLayerCount)

    //TODO for each NN in generation: run 1 "game" and evaluate.

    for(i <- 1 to 5){
      for (neuralNetwork <- generation.networks) {
        val lInput = new NEATInputProvider(neuralNetwork)
        val score = evaluate(lInput, rInput)
        neuralNetwork.score = score
        if(score > 0) logger.info(s"Killed prototype. score = $score")
      }

      generation.evolve
    }
  }

  def evaluate(lInput: PlayerInputProvider, rInput: PlayerInputProvider) = {
    val gameProps = new GameProperties()
    val physProps = new PhysicsProperties()
    val s = new GameState(gameProps, physProps, lInput, rInput)
    var latestScore = s.getMyScore
    var stepCounter = 0
    var latestPointGainedStep = 0

    while ((stepCounter - latestPointGainedStep) < maxIdleSteps) {
      s.step()

      if (s.getMyScore > latestScore) {
        latestPointGainedStep = stepCounter
        latestScore = s.getMyScore
        println(s"$stepCounter: $latestScore")
      }

      stepCounter = stepCounter + 1
    }

    latestScore
  }

}
