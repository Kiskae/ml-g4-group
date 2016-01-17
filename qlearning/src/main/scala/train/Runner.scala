package train

import server._
import qfunc._

object Runner {
  def apply(gameProps:GameProperties, physProps:PhysicsProperties, s:String):Runner = {
    println(s"Runner: $s")
    s.toLowerCase match {
      case "centerrand" => new CenterRandRunner(gameProps, physProps)
      case "rand" => new RandRunner(gameProps, physProps)
      case _ => throw new IllegalArgumentException()
    }
  }
}

trait Runner {
  def run[SType](state:carlo.TrainingGameState, qFunc:QFunction[SType], qAgent:QFunctionInputProvider):(Int,Seq[(SType,Int)])
}
