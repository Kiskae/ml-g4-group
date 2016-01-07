package qlearning

import java.io.File

import agent.{PlayerInput => Input}
import server.{GameStateInterface => State, _}
import ui.SwingUI

object QAgentUI extends App {
  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()

  val qAgent = new TableQLearner(gameProps,physProps,randChance=0,alpha=0)
  val ui:SwingUI = new SwingUI(gameProps)
  val s:GameState = new GameState(gameProps, physProps, qAgent, qAgent)

  ui.init(s)
  while (true) {
    qAgent.loadFromFile(new File("QTable.tsv"))
    while (!s.isFinished()) {
      s.step()
      ui.display(s)
      Thread.sleep(5)
    }
    s.reset()
  }
}
