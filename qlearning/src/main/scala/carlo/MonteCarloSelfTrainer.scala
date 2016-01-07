package carlo

import server._
import scala.collection.mutable
import java.io.File

object MonteCarloSelfTrainer extends App {
  def runMatch():(Int,mutable.Set[(Int,Int)],mutable.Set[(Int,Int)]) = {
    // (stateNdx,actionNdx)
    val lHistory = new mutable.HashSet[(Int,Int)]()
    val rHistory = new mutable.HashSet[(Int,Int)]()

    s.reset()
    while (!m.matchFinished) {
      // This could be dangerous, but qAgent doesn't save the state so it should be okay
      val lStateNdx = qTable.stateNdx(s)
      val lActionNdx = qAgent.policy(s)
      val lInput = QTableInputProvider.inputs(lActionNdx)

      val rState = new GameStateSnapshot(s, true)
      val rStateNdx = qTable.stateNdx(rState)
      val rActionNdx = qAgent.policy(rState)
      val rInput = QTableInputProvider.inputs(rActionNdx)

      m.step(lInput, rInput)
      lHistory += ((lStateNdx, lActionNdx))
      rHistory += ((rStateNdx, rActionNdx))
    }

    val reward = if (s.lScore > 0) 1 else -1

    (reward, lHistory, rHistory)
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
  val s = new TrainingGameState(gameProps, physProps, qAgent, qAgent);
  val m = s.`match`
  val r = scala.util.Random

  val trainingEpochs = 10000
  var hits = 0
  var epochCount = 0

  while (true) {
    val (result, lHistory, rHistory) = runMatch()
    updateQTable(result,lHistory,0.1)
    updateQTable(-result,rHistory,0.1)
    s.changeServer()

    hits += s.lHits + s.rHits
    epochCount += 1

    if (epochCount >= trainingEpochs) {
      println(f"Completed epoch. hits=${hits.toFloat/trainingEpochs}%6f randChance=${qAgent.randChance}%6f qSum=${qTable.table.map(_.max).sum}%.0f")
      epochCount = 0
      hits = 0
      qAgent.randChance *= .91
      qTable.writeToFile(qTableFile)
    }
  }
}
