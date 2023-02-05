package higherOrderFunctions.JsonToCSV

import spray.json._

import java.io.{File, PrintWriter}
import scala.io.Source

object Problem4 extends App {
  case class User(userId: Int, firstName: String, lastName: String, phoneNumber: String, emailAddress: String)
  private object UserJsonFormat extends DefaultJsonProtocol{
    implicit val userFormat: RootJsonFormat[User] = jsonFormat5(User)
  }
  import UserJsonFormat._
  private val jsonString = Source.fromFile("/home/anand/Downloads/json/example4.json").mkString
  val jsonAst = jsonString.parseJson
  private val users = jsonAst.asJsObject.getFields("users") match {
    case Seq(JsArray(users)) => users.map(_.convertTo[User])
    case _ => throw DeserializationException("Expected object with 'users' field")
  }
  private val csv = users.map(u => s"${u.userId},${u.firstName},${u.lastName},${u.phoneNumber},${u.emailAddress}").mkString("\n")
  println(csv)

  val writer = new PrintWriter(new File("data.csv"))
  writer.write("userId, firstName, lastName, phoneNumber, emailAddress\n")
  writer.write(csv)
  writer.close()
}
