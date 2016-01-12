package train

import qfunc.QFunction
import server._

object Trainer {
  def apply(gameProps:GameProperties, physProps:PhysicsProperties, s:String):Trainer = {
    s.toLowerCase match {
      case "carlo" => new MonteCarloTrainer()
      //case "qlearning" => new QLearningTrainer()
      case _ => throw new IllegalArgumentException()
    }
  }
}

trait Trainer {
  //TODO: Support multiple rewards
  def train[SType](reward:Int, history:Seq[(SType,Int)], qFunc:QFunction[SType])
}

class MonteCarloTrainer extends Trainer {
  def train[SType](reward:Int, history:Seq[(SType,Int)], qFunc:QFunction[SType]) {
    for ((state,action) <- Set()++history) {
      qFunc.updateRepr(state,action,reward)
    }
  }
}
