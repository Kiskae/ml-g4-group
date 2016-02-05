package train

import qfunc._

class CompositeRunner(a:Runner, b:Runner, aProbability:Double) extends Runner {
  val r = scala.util.Random
  override def run[SType](s:TrainingGameState, qFunc:QFunction[SType], qAgent:QFunctionInputProvider) = {
    if (r.nextDouble < aProbability) {
      a.run(s,qFunc,qAgent)
    } else {
      b.run(s,qFunc,qAgent)
    }
  }
}
