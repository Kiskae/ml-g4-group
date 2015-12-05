import agent.{PlayerInput, AlwaysLeftInputProvider}
import server.GameStateInterface

/**
  * Created by bas on 5-12-15.
  */
class NEATInputProvider extends AlwaysLeftInputProvider{
  override def getInput(gameStateInterface: GameStateInterface): PlayerInput = {
    val left = true
    val right = false
    val up = true
    new PlayerInput(left, right, up)


  }
}
