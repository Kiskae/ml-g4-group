package train

import qfunc.QFunction

object Trainer {
  def apply(s:String):Trainer = {
    s.toLowerCase match {
      //case "carlo" => new MonteCarloTrainer()
      //case "qlearning" => new QLearningTrainer()
      case _ => throw new IllegalArgumentException()
    }
  }
}

trait Trainer {
  //TODO: Support multiple rewards
  def train[SType](reward:Int, history:Seq[SType], qfunc:QFunction[SType])
}
