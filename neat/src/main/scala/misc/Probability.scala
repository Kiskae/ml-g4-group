package misc

import scala.util.Random

class Probability(private val max: Int) {
  def test(r: Random): Boolean = r.nextInt(max) == 0
}

object Probability {
  def apply(probability: Double): Probability = {
    new Probability((1 / probability).toInt)
  }
}
