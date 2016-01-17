package train

import server._
import qfunc._
import java.io.File

object QTrainMain extends App {
  require(args.length == 4, "Must take four args: <qtype> <qfile> <runner> <trainer>")
  println(args.mkString(" "))

  val gameProps = new GameProperties()
  val physProps = new PhysicsProperties()

  val qFuncFile = new File(args(1))
  val qFunc = QFunction(gameProps, physProps, args(0), qFuncFile)
  val runner = Runner(gameProps, physProps, args(2))
  val trainer = Trainer(gameProps, physProps, args(3))

  val qAgent = new QFunctionInputProvider(qFunc, randChance=0.1)
  val state = new carlo.TrainingGameState(gameProps, physProps, qAgent, qAgent)

  // Training parameters
  val matchesPerEpoch = 10000
  val epochsPerRun = 10000
  val annealFactor = .999
  val minRand = .0005

  var reward = 0
  var epochCount = 0

  for (_ <- 1 to epochsPerRun) {
    for (_ <- 1 to matchesPerEpoch) {
      val (result, history) = runner.run(state, qFunc, qAgent)
      trainer.train(result, history, qFunc)
      reward += result
    }

    println(f"${reward.toFloat/matchesPerEpoch}%.4f,${qAgent.randChance}%6f")
    epochCount = 0
    reward = 0
    if (qAgent.randChance > minRand) {
      qAgent.randChance *= annealFactor
    } else {
      qAgent.randChance = minRand
    }
  }
  qFunc.writeToFile(qFuncFile)
}
