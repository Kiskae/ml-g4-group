package train

import server.GameState
import qfunc._

object Runner {
  def apply(s:String):Runner = {
    s.toLowerCase match {
      //case "self" => new SelfRunner()
      case _ => throw new IllegalArgumentException()
    }
  }
}

trait Runner {
  def run[SType](state:GameState, qFunc:QFunction[SType], agent:QFunctionInputProvider):(Int,Seq[(SType,Int)])
}
