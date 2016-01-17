package train

import qfunc.QFunction
import server._

object Trainer {
  def apply(gameProps:GameProperties, physProps:PhysicsProperties, s:String):Trainer = {
    println(s"Trainer: $s")
    s.toLowerCase match {
      case "carlo"        => new MonteCarloTrainer()
      case "qlearning"    => new QLearningTrainer()
      case "revqlearning" => new RevQLearningTrainer()
      case "sarsa"        => new SarsaTrainer()
      case "revsarsa"     => new RevSarsaTrainer()
      case _ => throw new IllegalArgumentException()
    }
  }
}

trait Trainer {
  //TODO: Support multiple rewards
  def train[SType](reward:Int, history:Seq[(SType,Int)], qFunc:QFunction[SType])
}

