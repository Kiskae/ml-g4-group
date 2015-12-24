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

  val maxIdleSteps = 5000
  val cutOffScore = 20

  def main(args: Array[String]) = {
    logger.info("Starting NEAT!")
    //    val lInput = new BallFollower(30000L / 2)

    train
    view

  }

  def view = {
    val rInput = new BallFollower(30000L / 2)

    val neuralNetwork = ReadObjectFromFile[NeuralNetwork]("network.obj")

    val lInput = new NEATInputProvider(neuralNetwork)
    val score = evaluate(lInput, rInput, true, neuralNetwork)
  }

  def ReadObjectFromFile[A](filename: String)(implicit m: scala.reflect.Manifest[A]): A = {
    val input = new ObjectInputStream(new FileInputStream(filename))
    val obj = input.readObject()
    obj match {
      case x if m.runtimeClass.isInstance(x) => x.asInstanceOf[A]
      case _ => sys.error("Type not what was expected when reading from file")
    }
  }

  def train = {
    val rInput = new BallFollower(30000L / 2)
    val generationCount = 2
    val speciesCount = 5
    val networksPerSpecies = 400
    val inputLayerCount = 6
    val outputLayerCount = 3
    val generation = NetworkCreator.generation(speciesCount, networksPerSpecies, inputLayerCount, outputLayerCount)

    //TODO for each NN in generation: run 1 "game" and evaluate.


    for (i <- 0 until generationCount) {
      generation.evolve
      println("Starting new generation!")
      for (neuralNetwork <- generation.networks) {
        val lInput = new NEATInputProvider(neuralNetwork)
        val score = evaluate(lInput, rInput, false, neuralNetwork)
        neuralNetwork.score = score
        if (score > 0) logger.info(s"Killed prototype. score = $score")
      }

      val bestPrototypes = generation.getBestPrototypes
      println(bestPrototypes)
      println(bestPrototypes.map(_.getWeights.length))
    }

    //store best network
    val bestNetwork = generation.networks.sortBy(x => x.score).last
    val oos = new ObjectOutputStream(new FileOutputStream("network.obj"))
    oos.writeObject(bestNetwork)
    oos.close

  }

  def evaluate(lInput: PlayerInputProvider, rInput: PlayerInputProvider, showUI: Boolean, network: NeuralNetwork) = {
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
          println(s"Our player reached score of $cutOffScore. weights: " + network.getWeights)
        }else if(s.getOpponentScore >= cutOffScore) {
          run = false
          println(s"Opponent reached score of $cutOffScore. weights: " + network.getWeights)
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
//    latestScore - s.getOpponentScore
    if(latestScore != s.getMyScore) sys.error("shouldnt happen")
    (latestScore - s.getOpponentScore)/ stepCounter.toFloat * 1000
  }
}
