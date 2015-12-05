import agent.{PlayerInputProvider, BallFollower}
import server.{GameProperties, PhysicsProperties, GameState, GameLoop}
import ui.SwingUI

/**
  * Created by bas on 5-12-15.
  */
object NEAT {

  def main(args: Array[String]) = {
    val gameProps = new GameProperties()
    val physProps = new PhysicsProperties()
    while (true) {
      val lInput = new BallFollower(gameProps.playerRadius / 2)
      val rInput = new BallFollower(gameProps.playerRadius / 2)
      val s = new GameState(gameProps, physProps, lInput, rInput)
      //         run(s);
      val ui = new SwingUI(gameProps)
      new GameLoop(60, ui, s).run()
    }
  }

}
