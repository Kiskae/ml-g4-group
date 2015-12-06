import agent.{PlayerInput => Input}
import server.{GameStateInterface => State}
import server.GameProperties
class TableQLearner extends AbstractQLearner {

  //Initialize Q
  //Q will grow over the course of the training. No need to initialize a full Q table.

  override def getQ(state: State, action: Int) = {

    //Retrieve Q from table

    0.0
  }

  override def getMaxAction(state: State) = {
    //Get possible actions from R

    //Retrieve maximum value from Q

    (1, 0.0)
  }

  override def updateQ(state: State, action: Int, nextState: State) = {

    // update the Q table

  }

  // Simulate the result function
  def R(state: State): Int = {
    // if ball coords are y = 0 and 0 > x < 10 than we return 100 else 0.

    0
  }
}

