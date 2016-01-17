package common

import java.util.AbstractList

//TODO: This should probably conform to some interface... Seq? IndexedSeq?
class CircularBuffer[E: scala.reflect.ClassTag](capacity:Int) {
  val arr = new Array[E](capacity)
  var size = 0
  var headNdx = 0

  private def physicalNdx(ndx:Int) = (ndx+headNdx)%size

  def length = size

  def apply(ndx:Int) = arr(physicalNdx(ndx))

  def set(ndx:Int, e:E) = {
    val out = arr(physicalNdx(ndx))
    arr(physicalNdx(ndx)) = e
    out
  }

  def append(e:E) {
    if (size < capacity) {
      arr(size) = e
      size += 1
    } else {
      arr(headNdx) = e
      headNdx += 1
      if (headNdx >= capacity) {
        headNdx = 0
      }
    }
  }
}
