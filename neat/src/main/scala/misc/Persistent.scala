package misc

import java.io.{FileInputStream, ObjectInputStream}

object Persistent {
  def ReadObjectFromFile[A](filename: String)(implicit m: scala.reflect.Manifest[A]): A = {
    val input = new ObjectInputStream(new FileInputStream(filename))
    val obj = input.readObject()
    obj match {
      case x if m.runtimeClass.isInstance(x) => x.asInstanceOf[A]
      case _ => sys.error("Type not what was expected when reading from file")
    }
  }
}
