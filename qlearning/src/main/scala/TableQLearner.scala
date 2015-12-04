import agent.{PlayerInput => Input}
import server.{GameStateInterface => State}

class TableQLearner extends AbstractQLearner {
  override def getQ(state: State, action: Int) = ???
  override def getMaxAction(state: State) = ???
  override def updateQ(state: State, action: Int, nextState: State) = ???
}

