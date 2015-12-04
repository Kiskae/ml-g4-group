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
  var prev:Option[Tuple2[State,Int]] = None

  override def getInput(state: State) = {
    val maxAction = getMaxAction(state)
    if (!prev.isEmpty) {
      updateQ(prev.get._1, prev.get._2, state)
    }

    prev = Some((state,maxAction._1))

    inputs(maxAction._1)
  }

  def getReward(state: State): Double = state.getMyHits*100 + state.getOpponentHits*100

  def getQ(state: State, action: Int): Double
  def getMaxAction(state: State): Tuple2[Int, Double]
  def updateQ(state: State, action: Int, nextState: State)
}
