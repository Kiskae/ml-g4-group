import java.io.File

import server._
import agent._
import qfunc._
import scala.util.Try

object ResultGen extends App {
  def getAgent(smallDir:File, abs:Boolean):PlayerInputProvider = {
    val qTable = if (!abs) new QTable(gameProps, physProps)
                 else new AbsoluteQTable(gameProps, physProps)
    qTable.loadFromFile(new File(smallDir.getAbsolutePath+"/QTable.tsv"))
    new QFunctionInputProvider(qTable, randChance=0)
  }

  def getAgents(largeDir:File):Array[PlayerInputProvider] = {
    val absolute = largeDir.getName.startsWith("abs")
    largeDir.listFiles.map(f => Try(getAgent(f,absolute))).filter(_.isSuccess).map(_.get)
  }

  // Does player l beat player r?
  def matchup(l:PlayerInputProvider, r:PlayerInputProvider):Boolean = {
    state.reset()
    var numSteps = 0
    val maxSteps = 1000000

    while (!m.matchFinished && numSteps < maxSteps) {
      val lInput = l.getInput(state,Side.LEFT)
      val rInput = r.getInput(state,Side.RIGHT)
      m.step(lInput, rInput)
      numSteps += 1
    }
    if (numSteps >= maxSteps) { println("maxSteps exceeded") }
    
    state.lScore > 0
  }

  // How many times does a player from a beat a player from b?
  def matchup2(a:Array[PlayerInputProvider], b:Array[PlayerInputProvider]):(Int,Int) = {
    var winCount = 0
    var matchCount = 0
    for (i <- a) {
      for (j <- b) {
        if (matchup(i,j)) winCount += 1
        if (!matchup(j,i)) winCount += 1
        matchCount += 2
      }
    }

    (winCount,matchCount)
  }

  require(args.length == 1, "Must take one arg: <runDir>")
  val runDir = new File(args(0))
  val files = runDir.listFiles

  val gameProps = new GameProperties()
  val physProps = new PhysicsProperties()
  val state = new carlo.TrainingGameState(gameProps, physProps, null, null)
  val m = state.`match`

  val agentNames = files.map(_.getName)
  val agents = files.map(getAgents(_))
  println(agents.map(_.size).sum)

  case class Trainer(qtable:String, runner:String, trainer:String)
  case class Agent(trainer:Trainer, runNumber:Int, provider:PlayerInputProvider)
  case class Match(server:Agent, reciever:Agent, serverWon:Boolean)

  import scala.collection.mutable
  val trainers = mutable.ArrayBuffer[Trainer]()
  val agentsC = mutable.ArrayBuffer[Agent]()
  val results = mutable.ArrayBuffer[Match]()
 
  for (i <- 0 until agents.size) {
    val name = agentNames(i).split("_")
    val trainer = Trainer(name(0),name(1),name(2))
    trainers += trainer
    for (j <- 0 until agents(i).size) {
      val agent = Agent(trainer, j+1, agent(i)(j))
      agentsC += agent
    }
  }

  for (i <- agentsC) {
    for (j <- agentsC) {
      results += Match(i, j, matchup(i.provider, j.provider))
    }
  }


  val results = Array.ofDim[(Int,Int)](agents.size,agents.size)

  for (i <- 0 until agents.size) {
    for (j <- 0 until agents.size) {
    //for (j <- i+1 until agents.size) {
      val (score, matches) = matchup(agents(i),agents(j))
      results(i)(j) = (score, matches)
      //results(j)(i) = (matches-score, matches)
    }
  }

  for (i <- 0 until agents.size) {
    print(agentNames(i)+"\t")
    print(results(i).map(_ match {case (a,b) => a.toDouble/b}).sum/agents.size)
    print("\t"+results(i).mkString("\t"))
    println()
  }
}
