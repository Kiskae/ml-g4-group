package qfunc

import server.{GameStateInterface => State}
import java.io.File

//object QFunction {
//  def apply(file:File) = {
//    val firstLine = new BufferedReader(new FileReader(file))
//    val src = Source.fromFile(file)
//    try {
//      src.getLines.find(_ => true) match {
//        case Some("QTable") => 
//        case Some("AbsoluteQTable") => 
//      }
//    } finally {
//      src.close()
//    }
//  }
//}

trait QFunction[SType] {
  def stateRepr(state: State):SType
  def qRow(stateRepr: SType):Array[Double]
  def update(stateRepr: SType, action: Int, newVal: Double):Unit

  def qRow(state: State):Array[Double] = qRow(stateRepr(state))
  def q(state: State, action: Int):Double = qRow(state)(action)
  def maxAction(state: State):Int = {
    val row = qRow(state)
    row.indexOf(row.max)
  }
  def update(state: State, action: Int, newVal: Double):Unit = update(stateRepr(state), action, newVal)
}
