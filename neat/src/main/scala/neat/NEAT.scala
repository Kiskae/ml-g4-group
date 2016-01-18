package neat

import java.io._
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import agent.{BallFollower, PlayerInput, PlayerInputProvider}
import com.github.tototoshi.csv.CSVWriter
import data.Generation
import grizzled.slf4j.Logging
import misc.Persistent
import mutation.NetworkCreator
import neural.{NeuralNetwork, Neuron, TrainingProvider}
import qfunc.{QFunction, QFunctionInputProvider}
import server.{GameProperties, GameState, PhysicsProperties, Side}
import ui.{SwingUI, UI}

import scala.math._

/**
  * Created by David on 7-1-2016.
  */
object NEAT extends Logging {
  val maxIdleSteps = 3000
  val cutOffScore = 5
  val initsPerEval = 50

  val gameProps = new GameProperties
  val physProps = new PhysicsProperties
  val resultsFile = new File("storage/results/Neat-vs-Ballfollower-v1.csv")
  val resultsSpeciesFile = new File("storage/results/Neat-vs-Ballfollower-v1-species.csv")

  def main(args: Array[String]): Unit = {
    gameProps.autoDropFrames = 1

    logger.info("Starting NEAT!")
    val qLearningOpponent = None // Uncomment for Qlearning: Some("storage/QTable.tsv/QTable.tsv")

    val networkName = train(
      None, //Some(Persistent.ReadObjectFromFile[Generation]("Generation-Exception-2016-01-17T18-23-36.obj")),
//      () => qLearningOpponent.map(loadQOpponent).getOrElse(new BallFollower(30000L / 2)),
      () => new BallFollower(30000L / 2),
      updateOpponentWithBestNetwork = false
    )
    //train(
    // Some(Persistent.ReadObjectFromFile[Generation]("Generation-2015-12-28T20:57:21.obj"),
    // new NEATInputProvider(Persistent.ReadObjectFromFile[NeuralNetwork]("network.obj")),
    // updateOpponentWithBestNetwork = true
    //)

    //    val networkName = "Best-Network-2016-01-16T12-42-02.obj"
    logger.info(s"New network: $networkName, viewing")
    view(networkName, opponent = qLearningOpponent.map(loadQOpponent))
    //view("Best-Network-2015-12-28T20:57:21.obj", Some("Best-Network-2015-12-28T20:57:21.obj"))
  }

  private def loadQOpponent(fileName: String): PlayerInputProvider = {
    new QFunctionInputProvider(QFunction(
      gameProps,
      physProps,
      "abstable",
      fileName
    ), randChance = 0.0)
  }

  def view(leftNetwork: String,
           rightNetwork: Option[String] = None,
           opponent: Option[PlayerInputProvider] = None
          ) = {
    val lInput = new NEATInputProvider(Persistent.ReadObjectFromFile[NeuralNetwork](leftNetwork))
    val rInput = rightNetwork
      .map(file => new NEATInputProvider(Persistent.ReadObjectFromFile[NeuralNetwork](file)))
      .orElse(opponent)
      .getOrElse(new BallFollower(30000L / 2))

    evaluate(gameProps, physProps, lInput, rInput, showUI = true)
  }

  def train(initialGeneration: Option[Generation],
            initialOpponent: () => PlayerInputProvider,
            updateOpponentWithBestNetwork: Boolean = false): String = {
    val trainingProvider = new TrainingProvider(initialOpponent)

    val writer = CSVWriter.open(resultsFile)
    writer.writeRow(List("generation", "highest score", "lowest score", "average score"))

    val speciesWriter = CSVWriter.open(resultsSpeciesFile)
//    speciesWriter.writeRow(List("generation") :: List.fill(30)())

    val generationCount = 400
    val speciesCount = 30
    val networksPerSpecies = 20
    val inputLayerCount = 6
    val outputLayerCount = 3

    initialGeneration.foreach(checkNeuronConsistency)
    initialGeneration.foreach(Neuron.ensureUniqueNeurons)

    val generation = initialGeneration.getOrElse(NetworkCreator.generation(
      speciesCount,
      networksPerSpecies,
      inputLayerCount,
      outputLayerCount
    ))

    try {
      for (i <- 0 until generationCount) {
        generation.evolve()
        generation.mutate(ThreadLocalRandom.current())

        logger.info(s"Starting generation $i/$generationCount.")

        generation.networks.par.foreach(neuralNetwork => {
          val lInput = new NEATInputProvider(neuralNetwork)
          neuralNetwork.score = evaluate(gameProps, physProps,
            lInput, trainingProvider.provider, showUI = false)
        })

        val bestPrototypes = generation.getBestPrototypes
        logger.info("Best prototypes: " + bestPrototypes)
        logger.info("Best prototypes.weights.length: " + bestPrototypes.map(_.getWeights.length))
        logger.info("Best prototypes.neurons.length: " + bestPrototypes.map(_.neurons.length))

        logger.info(s"GRAPH: [${generation.networks.map(_.score).mkString(",")}]")

        // Update the best network.
        if (updateOpponentWithBestNetwork) {
          val serializedBestNetwork = {
            val bos = new ByteArrayOutputStream()
            val oos = new ObjectOutputStream(bos)
            oos.writeObject(generation.networks.sortBy(x => x.score).last)
            oos.close()
            bos.toByteArray
          }

          //Hacky way of doing things, but at least it won't share data
          trainingProvider.setProvider(() => {
            new NEATInputProvider(new ObjectInputStream(
              new ByteArrayInputStream(serializedBestNetwork)
            ).readObject().asInstanceOf[NeuralNetwork])
          })
        }

        if (i % 100 == 0 && i != 0) {
          storeGenerationAndNetwork(generation)
        }

        writer.writeRow(List(i, generation.highestScore, generation.lowestScore, generation.averageScore))
        speciesWriter.writeRow(generation.species.map(_.getBestNetwork.score))

        generation.breed(ThreadLocalRandom.current())

        // SharedNodeCheck.check[NeuralNetwork, Neuron](generation.networks, _.neurons)
      }

      writer.close
      speciesWriter.close
    } catch {
      case th: Exception =>
        Persistent.WriteWithTimestamp({ ts => s"Generation-Exception-$ts.obj" }, { oos =>
          oos.writeObject(generation)
        })
        throw th
    }

    val bestNetwork = generation.networks.sortBy(x => x.score).last

    logger.info("Best network.neurons.length: " + bestNetwork.neurons.length)

    storeGenerationAndNetwork(generation)
  }

  def storeGenerationAndNetwork(generation: Generation): String = {
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

  def evaluate(gameProps: GameProperties,
               physProps: PhysicsProperties,
               lInput: PlayerInputProvider,
               rInput: PlayerInputProvider,
               showUI: Boolean) = {
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

  def evaluateRandomBallInits(lInput: PlayerInputProvider, showUI: Boolean) = {
    var score = 0
    val gameProps = new GameProperties()
    val physProps = new PhysicsProperties()
    val s = new GameState(gameProps, physProps, lInput, null)

    for (i <- 1 until initsPerEval) {
      score += run(s, gameProps, physProps, lInput)
    }

    score.toFloat
  }

  def run[SType](s: GameState, gameProps: GameProperties, physProps: PhysicsProperties, lInput: PlayerInputProvider) = {
    val emptyInput = new PlayerInput(false, false, false)
    val m = s.`match`
    val histMaxSize = 1000000
    var historyCount = 0

    do {
      setupMatch(s, gameProps, physProps)
      val setup = getSetup(s)
      var crossNet = false
      while ((!crossNet || m.ball.pCircle.posX <= 0) && !m.matchFinished && historyCount < histMaxSize) {
        crossNet |= m.ball.pCircle.posX <= 0
        //        val stateNdx = qFunc.stateRepr(s)
        //        val actionNdx = qAgent.policy(s)

        val rInput = emptyInput

        m.step(lInput.getInput(s, Side.LEFT), rInput)

        //        val toInsert = (stateNdx, actionNdx)
        //        if (history.isEmpty || history.last != toInsert) history += toInsert
      }
      if (historyCount >= histMaxSize) {
        println(s"setup:$setup hits:${s.lHits}")
      }

      historyCount += 1
    } while ((s.lHits == 0 && s.rScore == 0) || historyCount >= histMaxSize)

    val reward = if (s.rScore > 0) -1 else 1

    reward
  }

  def setupMatch(s: GameState, gameProps: GameProperties, physProps: PhysicsProperties) {
    s.reset()
    val r = ThreadLocalRandom.current()
    val ball = s.`match`.ball
    val pc = ball.pCircle
    pc.posX = gameProps.ballRadius + r.nextInt((gameProps.sideWidth - 2 * gameProps.ballRadius).toInt)
    pc.posY = physProps.playerMaxHeight / 4 + r.nextInt(3 * physProps.playerMaxHeight.toInt / 4)

    val angle = r.nextDouble * Pi
    pc.velY = round(physProps.playerCollisionVelocity * sin(angle))
    pc.velX = round(physProps.playerCollisionVelocity * cos(angle))

    s.`match`.lPlayer.pCircle.posX = -(gameProps.ballRadius + r.nextInt((gameProps.sideWidth - 2 * gameProps.ballRadius).toInt))
    ball.firstHit = false
  }

  def getSetup(s: GameState) = {
    val pc = s.`match`.ball.pCircle
    (pc.posX, pc.posY, pc.velY, pc.velX, s.`match`.lPlayer.pCircle.posX)
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

  def checkNeuronConsistency(gen: Generation): Unit = {
    val inconsisent = gen.networks.flatMap(_.neurons).groupBy(_.label).map { neurons =>
      //all neurons have the same label, so they must also have the same layer
      val layers = neurons._2.map(_.layer).distinct
      if (layers.length > 1) {
        logger.warn(s"Label ${neurons._1} has more than 1 layer: $layers")
        true
      } else {
        false
      }
    }.foldLeft(false) {
      _ | _
    }

    assert(!inconsisent, "Generation is inconsistent")
  }
}
