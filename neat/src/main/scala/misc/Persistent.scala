package misc

import java.io._
import java.time.LocalDateTime

object Persistent {
  private val STORAGE_DIRECTORY = new File("storage")

  def ReadObjectFromFile[A](filename: String)(implicit m: scala.reflect.Manifest[A]): A =
    ReadObjectFromFile(new File(STORAGE_DIRECTORY, filename))

  def ReadObjectFromFile[A](file: File)(implicit m: scala.reflect.Manifest[A]): A = {
    val input = new ObjectInputStream(new FileInputStream(file))
    val obj = input.readObject()
    obj match {
      case x if m.runtimeClass.isInstance(x) => x.asInstanceOf[A]
      case _ => sys.error(s"Type ${obj.getClass} not what was expected when reading from file")
    }
  }

  def WriteWithTimestamp(name: (String) => String, writer: (ObjectOutputStream) => Unit) = {
    STORAGE_DIRECTORY.mkdirs()
    val timestamp = LocalDateTime.now().withNano(0).toString.replace(':', '-')
    val file = new File(STORAGE_DIRECTORY, name(timestamp))
    file.createNewFile() //Ensure the file exists
    Loan.loan(new FileOutputStream(file)).to { fos =>
      val oos = new ObjectOutputStream(fos)
      writer(oos)
      oos.close()
    }
  }
}
