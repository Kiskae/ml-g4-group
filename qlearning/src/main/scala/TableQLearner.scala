import server._
import agent.{PlayerInput => Input}
import server.{GameStateInterface => State}
import ui.SwingUI

class TableQLearner(gameProps:GameProperties, gamma:Double=0.8, alpha:Double=0.9, randChance:Double=0.1) extends AbstractQLearner {
  //Initialize Q
  val qTable = Array.ofDim[Double](200,4,100,4,6)

  val r = scala.util.Random

  def mapX(x:Long) = 10+math.floor(x*10/(gameProps.sideWidth*2.0) min 9 max -10).toInt
  def mapY(y:Long) = (y*10/(gameProps.netHeight*3) min 9 max 0).toInt
  def mapVel(velX:Long,velY:Long) = (if (velX <= 0) 0 else 1) + (if (velY <= 0) 0 else 2)

  def qRow(state: State):Array[Double] = {
    val ballX = mapX(state.getBall.getPosX)
    val ballY = mapY(state.getBall.getPosY)
    val ballV = mapVel(state.getBall.getVelY,state.getBall.getVelX)

    val meX = mapX(state.getMe.getPosX)
    val meY = mapY(state.getMe.getPosY)
    val meV = mapVel(state.getMe.getVelY,state.getMe.getVelX)

    if (ballX*10 + ballY >= 200 || meX*10 + meY >= 100) {
      println(state.getBall.getPosX+" "+state.getBall.getPosY)
      println(ballX +" "+ ballY)
      println(state.getMe.getPosX+" "+state.getMe.getPosY)
      println(meX +" "+ meY)
    }

    val tmp = qTable(ballX*10 + ballY)(ballV)
    tmp(meX*10 + meY)(meV)
  }

  override def q(state: State, action: Int) = qRow(state)(action)

  override def maxAction(state: State) = {
    val row = qRow(state)
    val max = row.max

    (row.indexOf(max), max)
  }

  override def action(state: State) = {
    val ran = r.nextDouble
    //println(ran < randChance)
    if (ran < randChance)
      r.nextInt(6)
    else
      maxAction(state)._1
  }

  override def updateQ(state: State, action: Int, nextState: State) {
    val row = qRow(state)
    row(action) = (1-alpha)*row(action) + alpha*(reward(nextState) + gamma*maxAction(nextState)._2)
  }

  override def reward(state: State) = {
    // if ball coords are y = 0 and 0 > x < 10 than we return 100 else 0.
    if (mapY(state.getBall.getPosY) > 0)
      0
    else if (mapX(state.getBall.getPosX) < 10)
      -100
    else
      100
  }
}

object Main3 extends App {
  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()

  val qAgent = new TableQLearner(gameProps)
  val bAgent = new agent.BallFollower(gameProps.playerRadius/2)
  val ui:SwingUI = new SwingUI(gameProps)
  val s:GameState = new GameState(gameProps, physProps, qAgent, bAgent);

  while (true) {
    1 to 1000 foreach { _ =>
      while (!s.isFinished()) {
        s.step()
      }
      s.reset()
    }
    new GameLoop(180, ui, s).run();
    println("finished epoch: "+(s.lScore-s.rScore)+" "+(s.rHits+s.lHits))
    s.reset()
  }
  println("finished")
}
