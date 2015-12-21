import java.util.concurrent.TimeUnit

import agent.{BallFollower, PlayerInputProvider}
import grizzled.slf4j.Logging
import mutation.NetworkCreator
import server.{GameProperties, GameState, PhysicsProperties}
import ui.SwingUI

/**
  * Created by bas on 5-12-15.
  */
object NEAT extends Logging {

  val maxIdleSteps = 500

  def main(args: Array[String]) = {
    logger.info("Starting NEAT!")
    //    val lInput = new BallFollower(30000L / 2)
    val rInput = new BallFollower(30000L / 2)

    val speciesCount = 5
    val networksPerSpecies = 10
    val inputLayerCount = 4
    val outputLayerCount = 3
    val generation = NetworkCreator.generation(speciesCount, networksPerSpecies, inputLayerCount, outputLayerCount)

    //TODO for each NN in generation: run 1 "game" and evaluate.

    generation.evolve

    for(i <- 1 to 10){
      for (neuralNetwork <- generation.networks) {
        val lInput = new NEATInputProvider(neuralNetwork)
        val score = evaluate(lInput, rInput, false)
        neuralNetwork.score = score
        if(score > 0) logger.info(s"Killed prototype. score = $score")
      }

      generation.evolve
    }
  }

  def evaluate(lInput: PlayerInputProvider, rInput: PlayerInputProvider, showUI: Boolean) = {
    val gameProps = new GameProperties()
    val physProps = new PhysicsProperties()
    val s = new GameState(gameProps, physProps, lInput, rInput)
    var latestScore = s.getMyScore
    var stepCounter = 0
    var latestPointGainedStep = 0
    val ui = new SwingUI(gameProps)
    if(showUI) ui.init(s)

    val maxFps = 80
    val NS_PER_SEC = TimeUnit.SECONDS.toNanos(1)
    val MS_PER_SEC = TimeUnit.SECONDS.toMillis(1);
    val framePeriod = NS_PER_SEC / maxFps;

    while ((stepCounter - latestPointGainedStep) < maxIdleSteps) {
      val beginTime = System.nanoTime();
      s.step()
      if(showUI) ui.display(s)

      if (s.getMyScore > latestScore) {
        latestPointGainedStep = stepCounter
        latestScore = s.getMyScore
        println(s"$stepCounter: $latestScore")
      }

      if(showUI) {
        val timeDiff = System.nanoTime() - beginTime
        var sleepTime = framePeriod - timeDiff
        if (sleepTime > 0) {
          try {
            val tmp = sleepTime * MS_PER_SEC
            val sleepTimeMs = tmp / NS_PER_SEC
            val frac = (NS_PER_SEC / MS_PER_SEC)
            val sleepTimeNs = (sleepTime % frac).toInt
            //System.err.printf("Sleeping for %dms + %dns\n",sleepTimeMs,sleepTimeNs);
            Thread.sleep(sleepTimeMs, sleepTimeNs)
          } catch {
            case ex: InterruptedException => {
              System.err.println("Sleep interrupted? " + ex)
            }
          }
        }

        while (sleepTime < 0) {
          System.err.println("Uh oh... catching up.")
          s.step()
          stepCounter = stepCounter + 1
          sleepTime += framePeriod
        }
      }

      stepCounter = stepCounter + 1
    }

    ui.dispose()
    latestScore
  }

}
