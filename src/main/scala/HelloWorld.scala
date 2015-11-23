import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object HelloWorld {
  def main(args: Array[String]) {
    println("Hello, world!")

    import scala.concurrent.ExecutionContext.Implicits.global

    Await.result(Future {
      println("Testing")
    }, Duration("5s"))
  }
}