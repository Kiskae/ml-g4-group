package carlo

import agent._
import agent.{PlayerInput => Input}
import server.{GameStateInterface => State}
object QTableInputProvider {
  val inputs = Array(
    new Input(false, false, false),
    new Input(false, false, true),
    new Input(false, true, false),
    new Input(false, true, true),
    new Input(true, false, false),
    new Input(true, false, true)
  )
}

class QTableInputProvider(val qTable: QTable, var randChance: Double = .01) extends AlwaysLeftInputProvider {

  val r = scala.util.Random
  
  override def getInput(state: State) = QTableInputProvider.inputs(policy(state))

  def policy(state: State) = {
    if (r.nextDouble < randChance)
      r.nextInt(6)
    else
      maxAction(state)
  }

  def maxAction(state: State) = {
    val row = qTable.qRow(state)

    row.indexOf(row.max)
  }
}
