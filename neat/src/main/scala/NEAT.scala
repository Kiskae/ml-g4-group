import java.io.{FileOutputStream, ObjectOutputStream}
import java.util.Calendar
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

  def main(args: Array[String]) = {
    logger.info("Starting NEAT!")

    val generation = Persistent.ReadObjectFromFile[Generation]("Generation-2015-12-28T20:57:21.obj")
    val bestNetwork: NeuralNetwork = Persistent.ReadObjectFromFile[NeuralNetwork]("network.obj")

    train(Some(generation), bestNetwork)
    view
  }

  def view = {
    val rneuralNetwork = Persistent.ReadObjectFromFile[NeuralNetwork]("Best-Network-2015-12-28T20:57:21.obj")
        val rInput = new BallFollower(30000L / 2)
//    val rInput = new NEATInputProvider(rneuralNetwork)
    val neuralNetwork = Persistent.ReadObjectFromFile[NeuralNetwork]("Best-Network-2015-12-28T20:57:21.obj")

    val lInput = new NEATInputProvider(neuralNetwork)
    val score = evaluate(lInput, rInput, true)
  }

  def train(initialGeneration: Option[Generation], initialTrainingNetwork: NeuralNetwork) = {
    val rInputBall = new BallFollower(30000L / 2)
    val generationCount = 10
    val speciesCount = 5
    val networksPerSpecies = 100
    val inputLayerCount = 6
    val outputLayerCount = 3

    val generation = initialGeneration match {
      case Some(generation) => generation
      case None => NetworkCreator.generation(speciesCount, networksPerSpecies, inputLayerCount, outputLayerCount)
    }

    var trainingNetwork = initialTrainingNetwork
    for (i <- 0 until generationCount) {
      generation.evolve

      val rInputNetwork = new NEATInputProvider(trainingNetwork)

      println(s"Starting generation $i/$generationCount.")

      generation.networks.par.foreach( neuralNetwork => {
        val lInput = new NEATInputProvider(neuralNetwork)
        neuralNetwork.score = evaluate(lInput, rInputNetwork, false)
      })

      // Update the best network.
      trainingNetwork = generation.networks.sortBy(x => x.score).last
      val bestPrototypes = generation.getBestPrototypes
      println("Best prototypes: " + bestPrototypes)
      println("Best prototypes.weights.length: " + bestPrototypes.map(_.getWeights.length))
      println("Best prototypes.neurons.length: " + bestPrototypes.map(_.neurons.length))
    }

    // Store best network.
    trainingNetwork = generation.networks.sortBy(x => x.score).last

    println("Best network.neurons.length: " + trainingNetwork.neurons.length)
    val oos = new ObjectOutputStream(new FileOutputStream("Best-Network-" + Persistent.sdf.format(Calendar.getInstance.getTime) + ".obj"))
    oos.writeObject(trainingNetwork)
    oos.close

    generation.storeToFile("Generation-" + Persistent.sdf.format(Calendar.getInstance.getTime) + ".obj")

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
        }else if(s.getOpponentScore >= cutOffScore) {
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
    (latestScore - s.getOpponentScore) / stepCounter.toFloat * 1000
  }
}
