import java.util.Scanner
import java.io.File
import scala.collection.mutable

object Results2 extends App {
  case class Match(serveTrainNdx:Int, serveSubNdx:Int, recieverTrainNdx:Int, recieverSubNdx:Int, serverWon:Int) {
    def recieverWon = 1-serverWon
  }

  require(args.length == 1, "Must take one arg: <resultFile>")
  val scan = new Scanner(new File(args(0)))
  scan.nextLine()
  val matches = mutable.ArrayBuffer[Match]()

  while (scan.hasNextInt()) {
    matches += Match(scan.nextInt(),scan.nextInt(),scan.nextInt(),scan.nextInt(),scan.nextInt())
  }


  {
    val serveMatches = matches.groupBy(m => (m.serveTrainNdx,m.serveSubNdx))
    val returnMatches = matches.groupBy(m => (m.recieverTrainNdx,m.recieverSubNdx))
    val serveScores = serveMatches.mapValues(_.map(_.serverWon).sum)
    val returnScores = returnMatches.mapValues(_.map(_.recieverWon).sum)
    val scores = serveScores.map(v => (v._1,( v._2+returnScores(v._1) ).toDouble / (serveMatches(v._1).size + returnMatches(v._1).size) ))

    println(scores.toList.sortBy(_._2).mkString("\n"))
  }

  {
    val classMatches = matches.groupBy(m => (m.serveTrainNdx,m.recieverTrainNdx))
    val classScores = classMatches.mapValues(list => list.map(_.serverWon).sum*100/list.size)

    val classes = (matches.map(_.serveTrainNdx).distinct ++ matches.map(_.recieverTrainNdx).distinct).distinct
    val classesSorted = classes.sortBy(c =>
      -(matches.filter(_.serveTrainNdx == c).map(_.serverWon).sum + matches.filter(_.recieverTrainNdx == c).map(_.recieverWon).sum)
    )

    println(classesSorted)
    
    for (i <- classesSorted) {
      for (j <- classesSorted) {
        val score1 = classScores.getOrElse((i,j), 0)
        val score2 = 100-classScores.getOrElse((j,i), 100)
        val score3 = (score1+score2)/2
        //print(s"$score1/$score2\t")
        print(s"$score3\t")
      }
      println()
    }
    println()
    for (i <- classesSorted) {
      for (j <- classesSorted) {
        val score1 = classScores.getOrElse((i,j), 0)
        val score2 = 100-classScores.getOrElse((j,i), 1)
        print(s"$score1/$score2\t")
      }
      println()
    }
  }
}
