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

  val qAgent = new SimpleQAgent(gameProps)
  val ui:SwingUI = new SwingUI(gameProps)

  while (true) {
    val lInput:PlayerInputProvider = qAgent
    val rInput:PlayerInputProvider = new IgnoreAgent(gameProps)
    val s:GameState = new GameState(gameProps, physProps, lInput, rInput);
    new GameLoop(600, ui, s).run();
  }
}

/**
  * TODO: Blegh this can be done better f this.
  * @param state
  * @param gameProps
  */
class TableState(state: State, gameProps:GameProperties) {
  val ballX:Int = ((state.getBall.getPosX/(gameProps.sideWidth*2).toDouble)*10).toInt
  val ballY:Int = ((state.getBall.getPosY/(gameProps.netHeight*3).toDouble)*10).toInt
  val meX:Int = ((state.getMe.getPosX/(gameProps.sideWidth*2).toDouble)*10).toInt
  val meY:Int = ((state.getMe.getPosY/(gameProps.netHeight*3).toDouble)*10).toInt

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

  override def equals(o: Any) = o match {
    case that: TableState => {
      that.ballX == this.ballX &&
      that.ballY == this.ballY &&
      that.meX == this.meX &&
      that.meY == this.meY &&
      that.ballV == this.ballV &&
      that.playerV == this.playerV
    }
    case _ => false
  }

  //TODO Really? Change this.
  override def hashCode = ("" + ballX + "" + ballY + "" + meX + "" + meY + "" + ballV + "" + playerV).hashCode

}

/**
  * Just a simple agent that passes the ball over the net only once.
  * @param gameProps
  */
class IgnoreAgent(gameProps:GameProperties) extends AlwaysLeftInputProvider {
  override def getInput(state: State): Input = {

    if(state.getBall.getPosX == -1*gameProps.ballInitX && state.getBall.getPosY == gameProps.ballInitY ) {
      return new Input(false, false, true)
    } else if(state.getBall.getPosY <= gameProps.ballInitY-100000 ) {
      return new Input(true, false, true)
    } else if (state.getBall.getPosY <= gameProps.ballInitY-90000) {
      return new Input(true, false, false)
    }

    new Input(false, false, false)
  }
}

class SimpleQAgent(gameProps:GameProperties)  extends AlwaysLeftInputProvider{

  //Initialize Q
  //Q will grow over the course of the training. No need to initialize a full Q table.
  var Q = Map[(TableState, Int), Double]()

  //Learning rate
  val gamma = 0.8

  // Used to select a random action
  val r = scala.util.Random

  //When this value get smaller we will chose action from the Q matrix more often
  var anneal = 1.0
  val lr = 0.001

  var prev:Option[Tuple2[TableState,Int]] = None

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
    val ts = new TableState(state, gameProps)

    //Get the high value based of the previous state.
    val maxAction = getMaxAction(ts)

    //Last action
    if (!prev.isEmpty) {
      updateQ(prev.get._1, prev.get._2, maxAction._1)
    }

    //Chose the next action.
    val randomA = actionSelect(ts)

    //Save the current state and action.
    prev = Some((ts, randomA))

    //Next random action
    inputs(randomA)
  }

  /**
    * This will select a random action or an action based on the Q matrix.
    * @param state
    * @return
    */
  def actionSelect(state: TableState): Int = {

    val d:Double = Math.random()

    //TODO Refactor this. Can be done better.
    if (d*anneal > 0.001) {
      val randomA = r.nextInt(6)
      return randomA
    } else {
      println("Returning Q action")
      return getMaxAction(state)._2
    }


  }

  def getQ(ts: TableState, action: Int): Double = {

    //Retrieve Q from table
    val currentQ = Q.get((ts, action))

    if(currentQ.isEmpty) {
      Q += ((ts, action) -> 0)
      return 0.0
    } else {
      return currentQ.get
    }

  }

  def getMaxAction(ts: TableState) = {

    //Retrieve maximum value from Q
    var maxQ: Array[Double] = Array[Double]()

    //Iterate through every action
    for(action <- 0 to 6) {
      maxQ = maxQ :+ (getQ(ts, action))
    }

    maxQ.zipWithIndex.maxBy(_._1)

  }

  def updateQ(state: TableState, action: Int, Qmax:Double) = {

    // update the Q table
    Q -= ((state, action))
    Q += ((state, action) -> (R(state) + gamma * Qmax))

  }

  // Simulate the result function
  // if ball coords are y = 0 and 0 > x < 10 than we return 100 else 0.
  def R(state: TableState): Double = {

    //This is where we score a point.
    //TODO: Can this be done differently?
    if ( state.ballX > 0 && state.ballY <= 0) {

      //For every winning state we find we anneal the result.
      anneal = anneal - anneal * lr
      println("Progress: " + anneal)

      return 0.1
    } else {
      return 0
    }
  }

}

