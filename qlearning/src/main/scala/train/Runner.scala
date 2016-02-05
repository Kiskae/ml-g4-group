package train

import server._
import qfunc._

object Runner {
  def apply(gameProps:GameProperties, physProps:PhysicsProperties, s:String):Runner = {
    println(s"Runner: $s")
    s.toLowerCase match {
      case "centerrand" => new CenterRandRunner(gameProps, physProps)
      case "rand" => new RandRunner(gameProps, physProps)
      case "ballfollower" => new BallFollowerRunner(gameProps, physProps)
      case "serve" => new ServeRunner(gameProps, physProps)
      case "composite" => new CompositeRunner(new ServeRunner(gameProps, physProps), new RandRunner(gameProps, physProps), 0.1)
      case "mean" => new CompositeRunner(new ServeRunner(gameProps, physProps), new MeanRandRunner(gameProps, physProps), 0.15)
      case _ => throw new IllegalArgumentException()
    }
  }
}

trait Runner {
  def run[SType](state:TrainingGameState, qFunc:QFunction[SType], qAgent:QFunctionInputProvider):(Int,Seq[(SType,Int)])
}
