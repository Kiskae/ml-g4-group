package train

import server._
import qfunc._

object QTrainMain extends App {
  require(args.length == 4)

  val gameProps = new GameProperties()
  val physProps = new PhysicsProperties()

  val qFuncFile = new java.io.File(args(1))
  val qFunc = QFunction(gameProps, physProps, args(0), qFuncFile)
  val runner = Runner(gameProps, physProps, args(2))
  val trainer = Trainer(gameProps, physProps, args(3))

  val qAgent = new QFunctionInputProvider(qFunc)
  val state = new carlo.TrainingGameState(gameProps, physProps, qAgent, qAgent)

  val trainingEpochs = 10000
  val saveDelay = 100
  var saveDelayCount = 0
  var hits = 0
  var reward = 0
  var epochCount = 0

  while (true) {
    val (result, history) = runner.run(state, qFunc, qAgent)
    trainer.train(result, history, qFunc)
    state.changeServer()

    hits += state.lHits + state.rHits
    reward += result
    epochCount += 1

    if (epochCount >= trainingEpochs) {
      println(f"Completed epoch. hits=${hits.toFloat/trainingEpochs}%.4f reward=${reward.toFloat/trainingEpochs}%.4f randChance=${qAgent.randChance}%6f")
      epochCount = 0
      hits = 0
      reward = 0
      //if (qAgent.randChance > .001) qAgent.randChance *= .99
      qAgent.randChance *= .99
      saveDelayCount += 1
      if (saveDelayCount >= saveDelay) {
        qFunc.writeToFile(qFuncFile)
        saveDelayCount = 0
      }
    }
  }
}
