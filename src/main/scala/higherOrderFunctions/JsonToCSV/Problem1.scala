package higherOrderFunctions.JsonToCSV

import spray.json._

import java.io.{File, PrintWriter}
import scala.io.Source

object Problem1 extends App {
  case class Expense(WHAT: String, AMOUNT: Double)
  case class Week(NUMBER: Int, EXPENSE: List[Expense])
  case class Data(WHO: String, WEEK: List[Week])

  object MyJsonProtocol extends DefaultJsonProtocol {
    // Use the DefaultJsonProtocol to convert the case class to/from JSON
    implicit val expenseFormat: RootJsonFormat[Expense] = jsonFormat2(Expense)
    implicit val weekFormat: RootJsonFormat[Week] = jsonFormat2(Week)
    implicit val dataFormat: RootJsonFormat[Data] = jsonFormat2(Data)
  }

  import MyJsonProtocol._
  private val json = Source.fromFile("/home/anand/Downloads/json/sample3.json").mkString
  val data = json.parseJson.convertTo[List[Data]]
  val csvData = data
    .flatMap(person => person.WEEK.flatMap(week => week.EXPENSE.map(expense => (person.WHO, week.NUMBER, expense.WHAT, expense.AMOUNT))))
    .map(t => t._1 + "," + t._2 + "," + t._3 + "," + t._4)
    .mkString("\n")
  println(csvData)
  val writer = new PrintWriter(new File("data.csv"))
  writer.write(csvData)
  writer.close()
}
