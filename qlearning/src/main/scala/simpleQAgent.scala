import server._
import agent._
import agent.{PlayerInput => Input}
import server.{GameStateInterface => State}
import ui.SwingUI

/**
  * Created by diederik on 5-12-15.
  */

object Main2 extends App {
  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()


  while (true) {
    val ui:SwingUI = new SwingUI(gameProps);
    val lInput:PlayerInputProvider = new simpleQAgent(gameProps)
    val rInput:PlayerInputProvider = new BallFollower(gameProps.playerRadius/2)
    val s:GameState = new GameState(gameProps, physProps, lInput, rInput);
    new GameLoop(60, ui, s).run();
  }
}

/**
  * TODO: Blegh this can be done better f this.
  * @param state
  * @param gameProps
  */
class tableState(state: State, gameProps:GameProperties) {
  val ballX:Int = ((state.getBall.getPosX/(gameProps.sideWidth*2))*200).toInt
  val ballY:Int = ((state.getBall.getPosY/(gameProps.netHeight*3))*100).toInt
  val meX:Int = ((state.getMe.getPosX/(gameProps.sideWidth*2))*200).toInt
  val meY:Int = ((state.getMe.getPosY/(gameProps.netHeight*3))*100).toInt

  var ballV:Boolean = false
  if(state.getBall.getVelY <= 0) {
    ballV = false
  } else {
    ballV = true
  }

  var playerV:Boolean = false

  if(state.getMe.getVelY <= 0) {
    playerV = false
  } else {
    playerV = true
  }

  println("X: " + ballX + " Y: " + ballY)
}

class simpleQAgent(gameProps:GameProperties)  extends AlwaysLeftInputProvider{

  //Initialize Q
  //Q will grow over the course of the training. No need to initialize a full Q table.
  var Q = Map[(tableState, Int), Double]()

  //Learning rate
  val gamma = 0.8

  // Used to select a random action
  val r = scala.util.Random

  var prev:Option[Tuple2[tableState,Int]] = None

  val inputs = Array(
    new Input(false, false, false),
    new Input(false, false, true),
    new Input(false, true, false),
    new Input(false, true, true),
    new Input(true, false, false),
    new Input(true, false, true)
  )

  override def getInput(state: State) = {

    //Convert the state to a table state.
    val ts = new tableState(state, gameProps)

    //Get the high value based of the previous state.
    val maxAction = getMaxAction(ts)

    //Last action
    if (!prev.isEmpty) {
      updateQ(prev.get._1, prev.get._2, maxAction)
    }

    //next random action
    val randomA = r.nextInt(6)

    //Save the current state and action.
    prev = Some((ts, randomA))

    //Choose next action at random.
    inputs(randomA)
  }


  def getQ(ts: tableState, action: Int): Double = {

    //Retrieve Q from table
    val currentQ = Q.get((ts, action))

    if(currentQ.isEmpty) {
      Q += ((ts, action) -> 0)
      return 0.0
    } else {
      return currentQ.get
    }

  }

  def getMaxAction(ts: tableState) = {

    //Retrieve maximum value from Q
    var maxQ: Array[Double] = Array[Double]()

    //Iterate through every action
    for(action <- 0 to 6) {
      maxQ = maxQ :+ (getQ(ts, action))
    }

    maxQ.zipWithIndex.maxBy(_._1)._1

  }

  def updateQ(state: tableState, action: Int, Qmax:Double) = {

    // update the Q table
    Q -= ((state, action))
    Q += ((state, action) -> (R(state) + gamma * Qmax))

  }

  // Simulate the result function
  // if ball coords are y = 0 and 0 > x < 10 than we return 100 else 0.
  def R(state: tableState): Int = {

    //This is where we score a point.
    if ( state.ballX > 10 && state.ballY <= 2) {
      println("Returning 100!!")
      return 100
    } else {
      return 0
    }
  }

}

