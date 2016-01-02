import agent._
import agent.{PlayerInput => Input}
import server.{GameStateInterface => State}

object AbstractQLearner {
  val inputs = Array(
    new Input(false, false, false),
    new Input(false, false, true),
    new Input(false, true, false),
    new Input(false, true, true),
    new Input(true, false, false),
    new Input(true, false, true)
  )
}

abstract class AbstractQLearner extends AlwaysLeftInputProvider {
  import AbstractQLearner.inputs
  var prev:Option[(State,Int)] = None
  var prevScore:Int = -1

  private def isGameInit(state: State) =
    prev == None || prevScore != state.getMyScore + state.getOpponentScore

  override def getInput(state: State) = {
    val act = action(state)

    if (!prev.isEmpty && !isGameInit(state)) {
      updateQ(prev.get._1, prev.get._2, state)
    }

    prev = Some((state,act))
    prevScore = state.getMyScore + state.getOpponentScore

    inputs(act)
  }

  def reward(state: State): Double
  def q(state: State, action: Int): Double
  def maxAction(state: State): Tuple2[Int, Double]
  def action(state: State): Int
  def updateQ(state: State, action: Int, nextState: State)
}
