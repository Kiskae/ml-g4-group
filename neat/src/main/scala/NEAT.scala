import java.util.concurrent.TimeUnit

import agent.{BallFollower, PlayerInputProvider}
import data.Generation
import grizzled.slf4j.Logging
import misc.Persistent
import mutation.NetworkCreator
import neural.NeuralNetwork
import server.{GameProperties, GameState, PhysicsProperties}
import ui.SwingUI

object NEAT extends Logging {
  val maxIdleSteps = 3000
  val cutOffScore = 5

  def main(args: Array[String]): Unit = {
    logger.info("Starting NEAT!")

    val networkName = train(
      None,
      new BallFollower(30000L / 2),
      updateOpponentWithBestNetwork = true
    )
    //train(
    // Some(Persistent.ReadObjectFromFile[Generation]("Generation-2015-12-28T20:57:21.obj"),
    // new NEATInputProvider(Persistent.ReadObjectFromFile[NeuralNetwork]("network.obj")),
    // updateOpponentWithBestNetwork = true
    //)

    logger.info(s"New network: $networkName, viewing")
    view(networkName)
    //view("Best-Network-2015-12-28T20:57:21.obj", Some("Best-Network-2015-12-28T20:57:21.obj"))
  }

  def view(leftNetwork: String, rightNetwork: Option[String] = None) = {
    val lInput = new NEATInputProvider(Persistent.ReadObjectFromFile[NeuralNetwork](leftNetwork))
    val rInput = rightNetwork
      .map(file => new NEATInputProvider(Persistent.ReadObjectFromFile[NeuralNetwork](file)))
      .getOrElse(new BallFollower(30000L / 2))

    evaluate(lInput, rInput, showUI = true)
  }

  def train(initialGeneration: Option[Generation],
            initialOpponent: PlayerInputProvider,
            updateOpponentWithBestNetwork: Boolean = false): String = {
    val generationCount = 10
    val speciesCount = 5
    val networksPerSpecies = 100
    val inputLayerCount = 6
    val outputLayerCount = 3

    val generation = initialGeneration.getOrElse(NetworkCreator.generation(
      speciesCount,
      networksPerSpecies,
      inputLayerCount,
      outputLayerCount
    ))

    var opponent = initialOpponent
    for (i <- 0 until generationCount) {
      generation.evolve()

      logger.info(s"Starting generation $i/$generationCount.")

      generation.networks.par.foreach(neuralNetwork => {
        val lInput = new NEATInputProvider(neuralNetwork)
        neuralNetwork.score = evaluate(lInput, opponent, showUI = false)
      })

      val bestPrototypes = generation.getBestPrototypes
      logger.info("Best prototypes: " + bestPrototypes)
      logger.info("Best prototypes.weights.length: " + bestPrototypes.map(_.getWeights.length))
      logger.info("Best prototypes.neurons.length: " + bestPrototypes.map(_.neurons.length))

      // Update the best network.
      if (updateOpponentWithBestNetwork) {
        opponent = new NEATInputProvider(generation.networks.sortBy(x => x.score).last)
      }
    }

    // Store best network.
    val bestNetwork = generation.networks.sortBy(x => x.score).last

    logger.info("Best network.neurons.length: " + bestNetwork.neurons.length)
    var networkName: String = "" //Store current network name for easy viewing
    Persistent.WriteWithTimestamp({ ts =>
      networkName = s"Best-Network-$ts.obj"
      networkName
    }, { oos =>
      oos.writeObject(bestNetwork)
    })

    Persistent.WriteWithTimestamp({ ts => s"Generation-$ts.obj" }, { oos =>
      oos.writeObject(generation)
    })

    networkName
  }

  def evaluate(lInput: PlayerInputProvider, rInput: PlayerInputProvider, showUI: Boolean) = {
    val gameProps = new GameProperties()
    val physProps = new PhysicsProperties()
    val s = new GameState(gameProps, physProps, lInput, rInput)
    var latestScore = s.getMyScore
    var stepCounter = 0
    var latestPointGainedStep = 0
    val ui = new SwingUI(gameProps)
    if (showUI) ui.init(s)

    val maxFps = 80
    val NS_PER_SEC = TimeUnit.SECONDS.toNanos(1)
    val MS_PER_SEC = TimeUnit.SECONDS.toMillis(1)
    val framePeriod = NS_PER_SEC / maxFps
    var run = true

    while ((stepCounter - latestPointGainedStep) < maxIdleSteps && run) {
      val beginTime = System.nanoTime()
      s.step()
      if (showUI) ui.display(s)

      if (s.getMyScore > latestScore) {
        latestPointGainedStep = stepCounter
        latestScore = s.getMyScore

        if (s.getMyScore >= cutOffScore) {
          run = false
        } else if (s.getOpponentScore >= cutOffScore) {
          run = false
        }
      }

      if (showUI) {
        val timeDiff = System.nanoTime() - beginTime
        var sleepTime = framePeriod - timeDiff
        if (sleepTime > 0) {
          try {
            val tmp = sleepTime * MS_PER_SEC
            val sleepTimeMs = tmp / NS_PER_SEC
            val frac = (NS_PER_SEC / MS_PER_SEC)
            val sleepTimeNs = (sleepTime % frac).toInt
            Thread.sleep(sleepTimeMs, sleepTimeNs)
          } catch {
            case ex: InterruptedException => {
              logger.error("Sleep interrupted? " + ex)
            }
          }
        }

        while (sleepTime < 0) {
          logger.warn("Uh oh... catching up.")
          s.step()
          stepCounter = stepCounter + 1
          sleepTime += framePeriod
        }
      }

      stepCounter = stepCounter + 1
    }

    ui.dispose()
    (latestScore - s.getOpponentScore) / stepCounter.toFloat * 1000
  }
}
