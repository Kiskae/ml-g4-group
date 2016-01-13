import server._
import agent._
import ui.SwingUI
import qfunc._

object VolleyUI extends App {
  def getAgent(agentType:String, arg:String) = {
    agentType.toLowerCase match {
      case "qagent" => new QFunctionInputProvider(QFunction(gameProps,physProps,"table",arg),randChance=0.0)
      case "absqagent" => new QFunctionInputProvider(QFunction(gameProps,physProps,"abstable",arg),randChance=0.0)
      case "neat" => new neat.NEATInputProvider(misc.Persistent.ReadObjectFromFile[neural.NeuralNetwork](arg))
      case "human" => new KeyboardInputProvider(ui, arg(0), arg(2), arg(1))
      case "ballfollower" => new BallFollower((gameProps.playerRadius/arg.toDouble).toLong)
    }
  }

  require(args.length == 4)

  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()
  val ui = new SwingUI(gameProps)

  val lAgent = getAgent(args(0),args(1))
  val rAgent = getAgent(args(2),args(3))

  val game = new GameState(gameProps, physProps, lAgent, rAgent)

  //val pc = game.`match`.ball.pCircle
  //pc.posX=239279
  //pc.posY=210710
  //pc.velY=5016
  //pc.velX=14243
  //game.`match`.lPlayer.pCircle.posX = -156666

  new GameLoop(60, ui, game).run()

}  
