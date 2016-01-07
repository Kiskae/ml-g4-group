package qlearning

import java.io.File

import agent.{PlayerInput => Input}
import server.{GameStateInterface => State, _}
import ui.SwingUI

object QAgentUI extends App {
  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()

  val qAgent = new TableQLearner(gameProps,physProps,randChance=0,alpha=0)
  val bAgent = new agent.BallFollower(gameProps.playerRadius/2)
  val ui:SwingUI = new SwingUI(gameProps)
  val s:GameState = new GameState(gameProps, physProps, qAgent, bAgent)

  ui.init(s)
  while (true) {
    try {
      qAgent.loadFromFile(new File("QTable.tsv"))
      while (s.lScore + s.rScore < 6) {
        s.step()
        ui.display(s)
        Thread.sleep(5)
      }
      s.reset()
    } catch {
      case _: Throwable => {
        println("Error")
        Thread.sleep(100)
      }
    }
  }
}
