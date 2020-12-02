import scala.io.Source

object ReadIt {
  def main(args: Array[String]): Unit = {
    val filename = "fileopen.scala"
    println("------ > inputs/ImmunizationData.json\n")
    for (line <- Source.fromFile("inputs/ImmunizationData.json").getLines) {
      println(line)
    }
  }
}
