package qlearning

object Main extends App {
  val QL = new qLearningExample;
  QL.start()
}

/**
  * Q learning for the following graph
  *
  *       0
  *     /  \
  *    /    \
  *    1    2
  *    \   /
  *     \ /
  *      3
  *      |
  *      |
  *      4
  */
class qLearningExample {

  // Used to select a random state
  val r = scala.util.Random

  //Learning rate
  val gamma = 0.8

  //Initialize R Matrix
  //var R: Array[Array[Int]] = Array(Array(-1, 0, 0, -1, -1), Array(0, -1, -1, 0, -1), Array(0, -1, -1, 0, -1), Array(-1, 0, 0, -1, 100), Array(-1, -1, -1, 0, 100))

  //R from the example -> http://mnemstudio.org/path-finding-q-learning-tutorial.htm
  var R: Array[Array[Int]] = Array(Array(-1, -1, -1, -1, 0, -1), Array(-1, -1, -1, 0, -1, 100), Array(-1, -1, -1, 0, -1, -1), Array(-1, 0, 0, -1, 0, -1), Array(0, -1, -1, 0, -1, 100), Array(-1, 0, -1, -1, 0, 100))

  //Initialize Q matrix
  var Q: Array[Array[Double]] = Array.ofDim[Double](6, 6)

  def start(): Unit = {

    println("started...")

    //How many epochs do we run?
    var epoch = 0

    while(epoch != 2000) {

      //Select random state
      var s = r.nextInt(R(0).length)

      //println("Start: " + s)
      //Run until we have found our end state.
      do {

        //Choose a random transition from the R matrix
        val possibleStates = R(s).zipWithIndex.filter(_._1 >= 0).map(_._2)
        val a = possibleStates(r.nextInt(possibleStates.length))

        //Update the Q
        val futureStates = R(a).zipWithIndex.filter(_._1 >= 0).map(_._2)
        Q(s)(a) = R(s)(a) + gamma *  futureStates.map(Q(a)).max

        //update s
        s = a

      } while(s != R(0).length-1)

      epoch += 1
      //println("EPOCH: " + epoch)
    }

    R foreach (x => {(x foreach ( y => print(y + " - "))); println("")} )
    println("------------------------")
    Q foreach (x => {(x foreach ( y => print(y + " - "))); println("")} )

  }


}

