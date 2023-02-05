package higherOrderFunctions.JsonToCSV.Problem

import spray.json._
import scala.io.Source
import java.io._


object WithoutUsingLib extends App {
  case class Person(id: Int, name: String, dob: Int)
  object MyJsonProtocol extends DefaultJsonProtocol {
    implicit val personFormat: RootJsonFormat[Person] = jsonFormat3(Person)
  }
  import MyJsonProtocol._
  private val jsonData = Source.fromFile("/home/anand/Downloads/json/example_2.json").mkString
  val people = jsonData.parseJson.convertTo[List[Person]]
  private val csvData = people.map(p => s"${p.id},${p.name},${p.dob}").mkString("\n")
  println(csvData)
  private val writer = new PrintWriter(new File("people.csv"))
  writer.write("id,name,dob\n")
  writer.write(csvData)
  writer.close()
}