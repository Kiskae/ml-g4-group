package carlo

import server._
import scala.collection.mutable
import java.io.File

object MonteCarloLearner extends App {
  def runMatch():(Int,mutable.Set[(Int,Int)]) = {
    // (stateNdx,actionNdx)
    val history = new mutable.HashSet[(Int,Int)]()

    s.reset()
    while (!m.matchFinished) {
      // This could be dangerous, but qAgent doesn't save the state so it should be okay
      val stateNdx = qTable.stateNdx(s)
      val qActionNdx = qAgent.policy(s)
      val qInput = QTableInputProvider.inputs(qActionNdx)
      val bInput = bAgent.getInput(s, Side.RIGHT)

      m.step(qInput, bInput)
      history += ((stateNdx, qActionNdx))
    }

    val reward = if (s.lScore > 0) 1 else -1

    (reward, history)
  }

  def updateQTable(reward:Int,history:mutable.Set[(Int,Int)],alpha:Double) {
    for ((state,action) <- history) {
      val row = qTable.table(state)
      row(action) = (1-alpha)*row(action) + alpha*reward
    }
  }

  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()

  val qTable = new QTable(gameProps,physProps)

  val qTableFile = new File("QTable.tsv")
  if (qTableFile.exists) {
    qTable.loadFromFile(qTableFile)
  }

  val qAgent = new QTableInputProvider(qTable)
  val bAgent = new agent.BallFollower(gameProps.playerRadius/2)
  val s = new TrainingGameState(gameProps, physProps, qAgent, bAgent);
  val m = s.`match`
  val r = scala.util.Random

  val trainingEpochs = 10000
  var maxHits = 0
  var epochCount = 0

  var resultSum = 0
  while (true) {
    bAgent.offset = r.nextInt((gameProps.playerRadius/2).toInt)+gameProps.playerRadius/4
    val (result, history) = runMatch()
    updateQTable(result,history,0.1)
    s.changeServer()

    resultSum += result
    epochCount += 1

    if (epochCount >= trainingEpochs) {
      println(f"Completed epoch. result=${resultSum.toFloat/trainingEpochs}%6f randChance=${qAgent.randChance}%6f qSum=${qTable.table.map(_.max).sum}%.0f")
      epochCount = 0
      resultSum = 0
      qAgent.randChance *= .91
      qTable.writeToFile(qTableFile)
    }
  }
}
