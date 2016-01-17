package train

import qfunc.QFunction

class QLearningTrainer(val gamma:Double = 1) extends Trainer {
  def train[SType](reward:Int, history:Seq[(SType,Int)], qFunc:QFunction[SType]) {
    var i = 0
    while (i < history.size-1) {
      val (state,action) = history(i)
      val (nextState,_) = history(i+1)
      qFunc.updateRepr(state,action,gamma*qFunc.maxQRepr(nextState))

      i += 1
    }

    val (lastState,lastAction) = history.last
    qFunc.updateRepr(lastState,lastAction,reward)
  }
}
