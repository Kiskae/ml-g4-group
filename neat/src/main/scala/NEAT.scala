import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.TimeUnit

import agent.{BallFollower, PlayerInputProvider}
import grizzled.slf4j.Logging
import mutation.NetworkCreator
import neural.NeuralNetwork
import server.{GameProperties, GameState, PhysicsProperties}
import ui.SwingUI

/**
  * Created by bas on 5-12-15.
  */
object NEAT extends Logging {

  val maxIdleSteps = 3000
  val cutOffScore = 5

  def main(args: Array[String]) = {
    logger.info("Starting NEAT!")

    train
    view
  }

  def view = {
    val rneuralNetwork = ReadObjectFromFile[NeuralNetwork]("network.obj")
//        val rInput = new BallFollower(30000L / 2)
    val rInput = new NEATInputProvider(rneuralNetwork)
    val neuralNetwork = ReadObjectFromFile[NeuralNetwork]("network.obj")

    val lInput = new NEATInputProvider(neuralNetwork)
    val score = evaluate(lInput, rInput, true)
  }

  def train = {
    val rInputBall = new BallFollower(30000L / 2)
    val generationCount = 50
    val speciesCount = 5
    val networksPerSpecies = 100
    val inputLayerCount = 6
    val outputLayerCount = 3
    val generation = NetworkCreator.generation(speciesCount, networksPerSpecies, inputLayerCount, outputLayerCount)
    var bestNetwork: NeuralNetwork = ReadObjectFromFile[NeuralNetwork]("network.obj")

    for (i <- 0 until generationCount) {
      generation.evolve

      val rInputNetwork = new NEATInputProvider(bestNetwork)

      println(s"Starting generation $i/$generationCount.")
      for (neuralNetwork <- generation.networks) {
        val lInput = new NEATInputProvider(neuralNetwork)
        var score = 0.0

//        if(i < 30){
//          score = evaluate(lInput, rInputBall, false)
//        }else{
          score = evaluate(lInput, rInputNetwork, false)
//        }

        neuralNetwork.score = score
      }

      // Update the best network
      bestNetwork = generation.networks.sortBy(x => x.score).last
      val bestPrototypes = generation.getBestPrototypes
      println("Best prototypes: " + bestPrototypes)
      println("Best prototypes.weights.length: " + bestPrototypes.map(_.getWeights.length))
      println("Best prototypes.neurons.length: " + bestPrototypes.map(_.neurons.length))
    }

    //store best network
    bestNetwork = generation.networks.sortBy(x => x.score).last

    println("Best network.neurons.length: " + bestNetwork.neurons.length)
    val oos = new ObjectOutputStream(new FileOutputStream("network.obj"))
    oos.writeObject(bestNetwork)
    oos.close

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
    val MS_PER_SEC = TimeUnit.SECONDS.toMillis(1);
    val framePeriod = NS_PER_SEC / maxFps;
    var run = true

    while ((stepCounter - latestPointGainedStep) < maxIdleSteps && run) {
      val beginTime = System.nanoTime();
      s.step()
      if (showUI) ui.display(s)

      if (s.getMyScore > latestScore) {
        latestPointGainedStep = stepCounter
        latestScore = s.getMyScore
//        println(s"$stepCounter: $latestScore")

        if (s.getMyScore >= cutOffScore) {
          run = false
//          println(s"Our player reached score of $cutOffScore. weights: " + network.getWeights)
        }else if(s.getOpponentScore >= cutOffScore) {
          run = false
//          println(s"Opponent reached score of $cutOffScore. weights: " + network.getWeights)
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


  def ReadObjectFromFile[A](filename: String)(implicit m: scala.reflect.Manifest[A]): A = {
    val input = new ObjectInputStream(new FileInputStream(filename))
    val obj = input.readObject()
    obj match {
      case x if m.runtimeClass.isInstance(x) => x.asInstanceOf[A]
      case _ => sys.error("Type not what was expected when reading from file")
    }
  }
}
