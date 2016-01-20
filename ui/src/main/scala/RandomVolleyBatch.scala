import server._
import agent._
import ui.SwingUI
import qfunc._

object RandomVolleyBatch extends App {
  def getAgent(agentType:String, arg:String) = {
    agentType.toLowerCase match {
      case "qagent" => new QFunctionInputProvider(QFunction(gameProps,physProps,"table",arg),randChance=0.0)
      case "absqagent" => new QFunctionInputProvider(QFunction(gameProps,physProps,"abstable",arg),randChance=0.0)
      case "neat" => new neat.NEATInputProvider(misc.Persistent.ReadObjectFromFile[neural.NeuralNetwork](arg))
      case "neat2" => IndependentNEATAgent.getAnAgent(arg)
      case "human" => new KeyboardInputProvider(ui, arg(0), arg(2), arg(1))
      case "ballfollower" => new BallFollower((gameProps.playerRadius/arg.toDouble).toLong)
    }
  }

  require(args.length == 4 || args.length == 5, "4 required arguments. Usage: <lAgentType> <lAgentArg> <rAgentType> <rAgentArg> [pointsToWin=21]")

  val gameProps:GameProperties = new GameProperties()
  val physProps:PhysicsProperties = new PhysicsProperties()
  val ui = new SwingUI(gameProps)

  val lAgent = getAgent(args(0),args(1))
  val rAgent = getAgent(args(2),args(3))
  val pointsToWin = if (args.length == 4) 21 else args(4).toInt

  val game = new RandomInitGameState(gameProps, physProps, lAgent, rAgent)

  while (true) {
    while (game.lScore < pointsToWin && game.rScore < pointsToWin) {
      game.step()
    }
    println(f"${game.lScore}%-3d - ${game.rScore}%3d")
    game.reset()
  }

}  
