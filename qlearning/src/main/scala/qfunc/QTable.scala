package qfunc

import server._
import server.{GameStateInterface => State, GameProperties => GameProps, PhysicsProperties => PhysProps}
import java.io.{PrintStream, File}
import java.util.Scanner

class QTable(gameProps:GameProps, physProps:PhysProps, alpha:Double = 0.1) extends QFunction[Int] {
  //Initialize Q
  lazy val table = Array.ofDim[Double](30*10*3*3,6)

  def mapX(x:Long) = 10+math.floor(x*10/(gameProps.sideWidth*2.0) min 19 max -10).toInt
  def mapY(y:Long) = (y*10/(gameProps.netHeight*3) min 9 max 0).toInt
  def mapVel(vel:Long) =  1+math.signum(vel / physProps.playerHorizontalSpeed).toInt

  /** Map a multi-dimensional array index to the eqivalent index of a flat array.
    *
    * The values of `nextDim` should range from 0 to `nextDimRange-1`.
    */
  def ndxAddDim(prevNdx:Int, nextDimRange:Int, nextDim:Int) = {
    require(nextDim >= 0 && nextDim < nextDimRange)
    prevNdx*nextDimRange + nextDim
  }

  override def stateRepr(state: State):Int = {
    val diffX = mapX(state.getBall.getPosX - state.getMe.getPosX)
    val diffY = mapY(state.getBall.getPosY - state.getMe.getPosY)
    val velY = mapVel(state.getBall.getVelY)
    val velX = mapVel(state.getBall.getVelX)

    ndxAddDim(ndxAddDim(ndxAddDim(diffX,10,diffY),3,velY),3,velX)
  }

  override def qRow(stateNdx: Int) = table(stateNdx)

  override def update(stateNdx: Int, action: Int, newVal: Double) {
    val row = qRow(stateNdx)
    row(action) = (1-alpha)*row(action) + alpha*newVal
  }

  override def writeToFile(file:File) {
    val stream = new PrintStream(file)
    table.foreach {row =>
      row.foreach(x => stream.print(f"$x%6f\t"))
      stream.println()
    }
    stream.close()
  }

  override def loadFromFile(file:File) {
    val scanner = new Scanner(file)
    for (i <- 0 until table.length) {
      for (j <- 0 until table(i).length) {
        table(i)(j) = scanner.nextDouble
      }
    }
    scanner.close()
  }
}
