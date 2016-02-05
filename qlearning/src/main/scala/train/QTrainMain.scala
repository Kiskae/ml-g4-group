package train

import server._
import qfunc._
import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar

object QTrainMain extends App {
  require(args.length == 4, "Must take four args: <qtype> <qfile> <runner> <trainer>")
  println(args.mkString(" "))

  val gameProps = new GameProperties()
  val physProps = new PhysicsProperties()

  val qFuncFile = new File(args(1))
  val qFunc = QFunction(gameProps, physProps, args(0), qFuncFile)
  val runner = Runner(gameProps, physProps, args(2))
  val trainer = Trainer(gameProps, physProps, args(3))

  val qAgent = new QFunctionInputProvider(qFunc, randChance=0)
  val state = new TrainingGameState(gameProps, physProps, qAgent, qAgent)

  // Training parameters
  val matchesPerEpoch = 10000
  val epochsPerSave = 100
  val annealFactor = .999
  val minRand = .0005

  val dateFormat = new SimpleDateFormat("HH:mm.ss")

  var reward = 0
  var epochCount = 0

  while (true) {
    for (_ <- 1 to epochsPerSave) {
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
    Console.err.print(s"${dateFormat.format(Calendar.getInstance.getTime)} Writing... ")
    qFunc.writeToFile(qFuncFile)
    Console.err.println(s"done. ${dateFormat.format(Calendar.getInstance.getTime)}")
  }
}
