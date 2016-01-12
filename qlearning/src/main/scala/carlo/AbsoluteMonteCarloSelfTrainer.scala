package carlo

import qfunc._
import server._
import scala.collection.mutable
import agent.PlayerInput
import java.io.File

object AbsolueMonteCarloSelfTrainer extends App {
  def setupMatch() {
    s.reset()
    val pCircle = m.ball.pCircle
    pCircle.posX = 0
    pCircle.posY = gameProps.netHeight + gameProps.ballRadius + r.nextInt(3*gameProps.netHeight.toInt)
    pCircle.velX = -r.nextInt(10000)-1
    pCircle.velY = 10000-r.nextInt(20000)
    m.ball.firstHit = false
  }

  def runMatch():(Int,mutable.Set[(Int,Int)]) = {
    // (stateNdx,actionNdx)
    val lHistory = new mutable.HashSet[(Int,Int)]()
    val emptyInput = new PlayerInput(false, false, false)
    val pc = m.ball.pCircle

    while (s.lHits == 0 && !m.matchFinished) {
      setupMatch()
      while (m.ball.pCircle.posX <= 0 && !m.matchFinished) {
        val lStateNdx = qTable.stateRepr(s)
        val lActionNdx = qAgent.policy(s)
        val lInput = QTableInputProvider.inputs(lActionNdx)

        val rInput = emptyInput

        m.step(lInput, rInput)
        lHistory += ((lStateNdx, lActionNdx))
      }
    }

    val reward = if (s.rScore > 0) -1 else 1
    //println(f"r:$reward ${pc.posX} ${pc.posY} ${s.lHits} ${s.rScore}")

    (reward, lHistory)
  }

  def updateQTable(reward:Int,history:mutable.Set[(Int,Int)],alpha:Double) {
    for ((state,action) <- history) {
      val row = qTable.table(state)
      row(action) = (1-alpha)*row(action) + alpha*reward
    }
  }

  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()

  val qTable = new AbsoluteQTable(gameProps,physProps)

  val qTableFile = new File("QTable_absolute.tsv")
  if (qTableFile.exists) {
    qTable.loadFromFile(qTableFile)
  }

  val qAgent = new QFunctionInputProvider(qTable)
  val s = new TrainingGameState(gameProps, physProps, qAgent, qAgent);
  val m = s.`match`
  val r = scala.util.Random

  val trainingEpochs = 10000
  var delay = 0
  var hits = 0
  var reward = 0
  var epochCount = 0

  while (true) {
    val (result, lHistory) = runMatch()
    updateQTable(result,lHistory,0.1)
    s.changeServer()

    hits += s.lHits + s.rHits
    reward += result
    epochCount += 1

    if (epochCount >= trainingEpochs) {
      println(f"Completed epoch. hits=${hits.toFloat/trainingEpochs}%.4f reward=${reward.toFloat/trainingEpochs}%.4f randChance=${qAgent.randChance}%6f qSum=${qTable.table.map(_.max).sum}%.0f")
      epochCount = 0
      hits = 0
      reward = 0
      //if (qAgent.randChance > .001) qAgent.randChance *= .99
      qAgent.randChance *= .99
      delay += 1
      if (delay >= 10) {
        qTable.writeToFile(qTableFile)
        delay = 0
      }
    }
  }
}

