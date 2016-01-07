package neat

import java.util.concurrent.TimeUnit

import agent.{BallFollower, PlayerInputProvider}
import data.Generation
import grizzled.slf4j.Logging
import misc.Persistent
import mutation.NetworkCreator
import neural.NeuralNetwork
import server.{GameProperties, GameState, PhysicsProperties}
import ui.{SwingUI, UI}

/**
  * Created by David on 7-1-2016.
  */
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
    val ui: Option[UI] = if (showUI) Some(new SwingUI(gameProps)) else None

    var stepCounter = 0
    val coreRunnable: (GameState) => Unit = ui.foldLeft({ gs: GameState =>
      s.step()
      stepCounter += 1
    })(uiWrapper)

    //Init UI if present
    ui.foreach(_.init(s))

    var latestPointGainedStep = 0
    var run = true

    while ((stepCounter - latestPointGainedStep) < maxIdleSteps && run) {
      val previousScore = s.getMyScore

      coreRunnable(s)

      if (s.getMyScore > previousScore) {
        latestPointGainedStep = stepCounter

        if (s.getMyScore >= cutOffScore) {
          run = false
        } else if (s.getOpponentScore >= cutOffScore) {
          run = false
        }
      }
    }

    //Destroy UI if present
    ui.foreach(_.finish(s))

    (s.getMyScore - s.getOpponentScore) / stepCounter.toFloat * 1000
  }

  private def uiWrapper(run: (GameState) => Unit, ui: UI): (GameState => Unit) = {
    val maxFps = 80
    val framePeriod = TimeUnit.SECONDS.toNanos(1) / maxFps

    (gs: GameState) => {
      val beginTime = System.nanoTime()

      run(gs)
      ui.display(gs)

      val timeDiff = System.nanoTime() - beginTime
      var sleepTime = framePeriod - timeDiff
      TimeUnit.NANOSECONDS.sleep(sleepTime)

      while (sleepTime < 0) {
        logger.warn("Uh oh... catching up.")
        gs.step()
        sleepTime += framePeriod
      }
    }
  }
}
