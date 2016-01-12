package train

import server._
import qfunc._

object QTrainMain extends App {
  require(args.length == 4)

  val gameProps = new GameProperties()
  val physProps = new PhysicsProperties()

  val qFile = new java.io.File(args(1))
  val qFunc = QFunction(gameProps, physProps, args(0), qFile)
  val runner = Runner(args(2))
  val trainer = Trainer(args(3))

  val qAgent = new QFunctionInputProvider(qFunc)
  val state = new GameState(gameProps, physProps, qAgent, qAgent)

  while (true) {
    val (reward, history) = runner.run(state, qFunc, qAgent)
    trainer.train(reward, history, qFunc)
  }
}
