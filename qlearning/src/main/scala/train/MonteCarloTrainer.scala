package train

import qfunc.QFunction

class MonteCarloTrainer extends Trainer {
  def train[SType](reward:Int, history:Seq[(SType,Int)], qFunc:QFunction[SType]) {
    for ((state,action) <- Set()++history) {
      qFunc.updateRepr(state,action,reward)
    }
  }
}
