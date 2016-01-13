package neural

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class TraceException(secondary: Boolean, root: Neuron) extends RuntimeException {
  var stack: mutable.Buffer[Neuron] = ArrayBuffer(root)
}
