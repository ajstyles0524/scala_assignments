package higherOrderFunctions.CSVToJson

import scala.io.Source
import spray.json._
import DefaultJsonProtocol._

object Problem1 extends App{

  case class Person(id: Int, name: String, dob: Int)
  implicit val personFormat: RootJsonFormat[Person] = jsonFormat3(Person)
  private val csvFile = Source.fromFile("/home/anand/Downloads/file.csv")
  private val lines = csvFile.getLines().drop(1) // drop the header line
  val people = lines.map { line =>
    val Array(id, name, dob) = line.split(",").map(_.trim)
    Person(id.toInt,name, dob.toInt)}.toList
  val jsonString = people.toJson.prettyPrint
  println(jsonString)
}
