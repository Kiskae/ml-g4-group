package qfunc

import server.{GameStateInterface => State, GameProperties => GameProps, PhysicsProperties => PhysProps}
import java.io.File

object QFunction {
  def apply(gameProps:GameProps, physProps:PhysProps, qtype:String, file:String):QFunction[_] = {
    apply(gameProps,physProps,qtype,new File(file))
  }

  def apply(gameProps:GameProps, physProps:PhysProps, qtype:String, file:File):QFunction[_] = {
    val func = qtype.toLowerCase match {
      case "table" => new QTable(gameProps, physProps)
      case "abstable" => new AbsoluteQTable(gameProps, physProps)
      case _ => throw new IllegalArgumentException()
    }

    if (file.exists) func.loadFromFile(file)

    func
  }
}

// Hack around Scala's generics
// http://stackoverflow.com/questions/3716260/covariant-typeparameter-in-scala-needs-to-be-invariant-in-java-interface
import scala.annotation.unchecked.uncheckedVariance

trait QFunction[+SType] {
  def stateRepr(state: State):SType
  def qRowRepr(stateRepr: SType @uncheckedVariance):Array[Double]
  def updateRepr(stateRepr: SType @uncheckedVariance, action: Int, newVal: Double):Unit
  def writeToFile(file:File):Unit
  def loadFromFile(file:File):Unit

  def qRow(state: State):Array[Double] = qRowRepr(stateRepr(state))
  def q(state: State, action: Int):Double = qRow(state)(action)
  def maxAction(state: State):Int = maxNdx(qRow(state))
  def update(state: State, action: Int, newVal: Double):Unit = updateRepr(stateRepr(state), action, newVal)

  private def maxNdx(arr:Array[Double]):Int = {
    var max = arr(0)
    var maxNdx = 0
    var i = 1

    while (i < arr.length) {
      if (arr(i) > max) {
        max = arr(i)
        maxNdx = i
      }
      i += 1
    }

    maxNdx
  }
}
