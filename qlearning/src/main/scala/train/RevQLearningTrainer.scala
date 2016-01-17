package train

import qfunc.QFunction

class RevQLearningTrainer(val gamma:Double = 1) extends Trainer {
  def train[SType](reward:Int, history:Seq[(SType,Int)], qFunc:QFunction[SType]) {
    val (lastState,lastAction) = history.last
    qFunc.updateRepr(lastState,lastAction,reward)

    var i = history.size-2
    while (i >= 0) {
      val (state,action) = history(i)
      val (nextState,_) = history(i+1)
      qFunc.updateRepr(state,action,gamma*qFunc.maxQRepr(nextState))

      i -= 1
    }
  }
}
