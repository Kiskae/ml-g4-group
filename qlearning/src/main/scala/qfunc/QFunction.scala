package qfunc

import server.{GameStateInterface => State, GameProperties => GameProps, PhysicsProperties => PhysProps}
import java.io.File

object QFunction {
  def apply(gameProps:GameProps, physProps:PhysProps, qtype:String, file:File):QFunction[Int] = {
    val func = qtype.toLowerCase match {
      case "qtable" => new QTable(gameProps, physProps)
      case "absqtable" => new AbsoluteQTable(gameProps, physProps)
      case _ => throw new IllegalArgumentException()
    }

    if (file.exists) func.loadFromFile(file)

    func
  }
}

trait QFunction[SType] {
  def stateRepr(state: State):SType
  def qRow(stateRepr: SType):Array[Double]
  def update(stateRepr: SType, action: Int, newVal: Double):Unit
  def writeToFile(file:File):Unit
  def loadFromFile(file:File):Unit

  def qRow(state: State):Array[Double] = qRow(stateRepr(state))
  def q(state: State, action: Int):Double = qRow(state)(action)
  def maxAction(state: State):Int = {
    val row = qRow(state)
    row.indexOf(row.max)
  }
  def update(state: State, action: Int, newVal: Double):Unit = update(stateRepr(state), action, newVal)
}
