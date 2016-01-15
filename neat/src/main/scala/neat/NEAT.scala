package neat

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import agent.{PlayerInput, BallFollower, PlayerInputProvider}
import data.Generation
import grizzled.slf4j.Logging
import misc.Persistent
import mutation.NetworkCreator
import neural.{NeuralNetwork, TrainingProvider}
import server.{Side, GameProperties, GameState, PhysicsProperties}
import ui.{SwingUI, UI}
import math._
import scala.collection.mutable

/**
  * Created by David on 7-1-2016.
  */
object NEAT extends Logging {
  val maxIdleSteps = 3000
  val cutOffScore = 5
  val initsPerEval = 50

  val processors = Runtime.getRuntime.availableProcessors()
  val r = scala.util.Random

  def main(args: Array[String]): Unit = {
    logger.info("Starting NEAT!")

    //    val generation = Persistent.ReadObjectFromFile[Generation]("Generation-2016-01-08T11-58-38.obj")

    val networkName = train(
      //      Some(generation),
      None,
      () => new BallFollower(30000L / 2),
      updateOpponentWithBestNetwork = false
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
            initialOpponent: () => PlayerInputProvider,
            updateOpponentWithBestNetwork: Boolean = false): String = {
    val trainingProvider = new TrainingProvider(initialOpponent)
    val generationCount = 200
    val speciesCount = 20
    val networksPerSpecies = 50
    val inputLayerCount = 6
    val outputLayerCount = 3

    val generation = initialGeneration.getOrElse(NetworkCreator.generation(
      speciesCount,
      networksPerSpecies,
      inputLayerCount,
      outputLayerCount
    ))

    for (i <- 0 until generationCount) {
      generation.evolve()
      generation.mutate(ThreadLocalRandom.current())

      logger.info(s"Starting generation $i/$generationCount.")

      generation.networks.par.foreach(neuralNetwork => {
        val lInput = new NEATInputProvider(neuralNetwork)
//        neuralNetwork.score = evaluate(lInput, trainingProvider.provider, showUI = false)
        neuralNetwork.score = evaluateRandomBallInits(lInput, showUI = false)
      })

      val bestPrototypes = generation.getBestPrototypes
      logger.info("Best prototypes: " + bestPrototypes)
      logger.info("Best prototypes.weights.length: " + bestPrototypes.map(_.getWeights.length))
      logger.info("Best prototypes.neurons.length: " + bestPrototypes.map(_.neurons.length))

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

      generation.breed(ThreadLocalRandom.current())

      // SharedNodeCheck.check[NeuralNetwork, Neuron](generation.networks, _.neurons)
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

  def evaluateRandomBallInits(lInput: PlayerInputProvider, showUI: Boolean) = {
    var score = 0
    val gameProps = new GameProperties()
    val physProps = new PhysicsProperties()
    val s = new GameState(gameProps, physProps, lInput, null)

    for(i <- 1 until initsPerEval){
      score += run(s, gameProps, physProps, lInput)
    }

    score.toFloat
  }

  def run[SType](s: GameState, gameProps:GameProperties, physProps: PhysicsProperties, lInput: PlayerInputProvider) = {
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

  def setupMatch(s:GameState, gameProps:GameProperties, physProps: PhysicsProperties) {
    s.reset()
    val ball = s.`match`.ball
    val pc = ball.pCircle
    pc.posX = gameProps.ballRadius + r.nextInt((gameProps.sideWidth-2*gameProps.ballRadius).toInt)
    pc.posY = physProps.playerMaxHeight/4 + r.nextInt(3*physProps.playerMaxHeight.toInt/4)

    val angle = r.nextDouble*Pi
    pc.velY = round(physProps.playerCollisionVelocity*sin(angle))
    pc.velX = round(physProps.playerCollisionVelocity*cos(angle))

    s.`match`.lPlayer.pCircle.posX = -(gameProps.ballRadius + r.nextInt((gameProps.sideWidth-2*gameProps.ballRadius).toInt))
    ball.firstHit = false
  }

  def getSetup(s:GameState) = {
    val pc = s.`match`.ball.pCircle
    (pc.posX,pc.posY,pc.velY,pc.velX,s.`match`.lPlayer.pCircle.posX)
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
