package demo

import java.io.File

import neat.NEATInputProvider
import qlearning.TableQLearner

object DemoMain extends App {
  //Hello Michael

  //NEAT example
  NEATInputProvider.readFromFile(new File("Best-Network-2016-01-06T20-24-47-good.obj"))

  //Probably breaking your code :P
  new TableQLearner(null, null)
}
