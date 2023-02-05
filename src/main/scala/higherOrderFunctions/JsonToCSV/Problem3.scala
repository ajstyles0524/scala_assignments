package higherOrderFunctions.JsonToCSV
import spray.json._

import java.io.{File, PrintWriter}
import scala.io.Source

case class Person(firstName: String, lastName: String, gender: String, age: Int, address: Address, phoneNumbers: List[PhoneNumber])
case class Address(streetAddress: String, city: String, state: String, postalCode: String)
case class PhoneNumber(`type`: String, number: String)

object MyJsonProtocol extends DefaultJsonProtocol{
  implicit val addressFormat: RootJsonFormat[Address] = jsonFormat4(Address)
  implicit val phoneNumberFormat: RootJsonFormat[PhoneNumber] = jsonFormat2(PhoneNumber)
  implicit val personFormat: RootJsonFormat[Person] = jsonFormat6(Person)
}

object Problem3 extends App{
  import MyJsonProtocol._
  private val json = Source.fromFile("/home/anand/Downloads/json/example3.json").mkString
  val person = json.parseJson.convertTo[Person]
  val csvData = s"${person.firstName},${person.lastName},${person.gender},${person.age},${person.address.streetAddress},${person.address.city},${person.address.state},${person.address.postalCode},${person.phoneNumbers.head.number}"
  val writer = new PrintWriter(new File("person.csv"))
  writer.write(csvData)
  writer.close()
}
