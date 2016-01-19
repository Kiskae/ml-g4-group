import java.util.Scanner
import java.io.File
import scala.collection.mutable

object Results2 extends App {
  case class Match(serveTrainNdx:Int, serveSubNdx:Int, recieverTrainNdx:Int, recieverSubNdx:Int, serverWon:Boolean)

  require(args.length == 1, "Must take one arg: <resultFile>")
  val scan = new Scanner(new File(args(0)))
  scan.nextLine()
  val matches = mutable.ArrayBuffer[Match]()

  while (scan.hasNextInt()) {
    matches += Match(scan.nextInt(),scan.nextInt(),scan.nextInt(),scan.nextInt(),scan.nextInt()==1)
  }

  val serveMatches = matches.groupBy(m => (m.serveTrainNdx,m.serveSubNdx))
  val returnMatches = matches.groupBy(m => (m.recieverTrainNdx,m.recieverSubNdx))
  val serveScores = serveMatches.mapValues(_.map(m => if (m.serverWon) 1 else 0).sum)
  val returnScores = returnMatches.mapValues(_.map(m => if (m.serverWon) 0 else 1).sum)
  val scores = serveScores.map(v => (v._1,v._2+returnScores(v._1)))
}
