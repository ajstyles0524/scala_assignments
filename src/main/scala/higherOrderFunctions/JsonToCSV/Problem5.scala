package higherOrderFunctions.JsonToCSV

import spray.json._
import java.io.{File, PrintWriter}
import scala.io.Source

case class Color(color: String, value: String)

case class Batter(id: String, `type`: String)
case class Topping(id: String, `type`: String)
case class Batters(batter: List[Batter])
case class Donut(id: String, `type`: String, name: String, ppu: Double, batters: Batter, topping: List[Topping])





object JsonProtocol extends DefaultJsonProtocol {
  implicit val colorFormat: RootJsonFormat[Color] = jsonFormat2(Color)
  //implicit val batterFormat: RootJsonFormat[Batter] = jsonFormat2(Batter)
  //implicit val toppingFormat: RootJsonFormat[Topping] = jsonFormat2(Topping)
  //implicit val battersFormat: RootJsonFormat[Batters] = jsonFormat1(Batters)
  //implicit val donutFormat: RootJsonFormat[Donut] = jsonFormat6(Donut)
}



object Problem5 extends App {

  // Problem - 1
  val json = """[ 100, 500, 300, 200, 400 ]"""
  val data = json.parseJson
  println(data)

  // Problem - 2
  import JsonProtocol._
  private val jsonData = Source.fromFile("/home/anand/Downloads/json/example5.json").mkString
  private val colors = jsonData.parseJson.convertTo[List[Color]]
  val csv = colors.map(u => s"${u.color},${u.value}").mkString("\n")
  println(csv)
  val writer = new PrintWriter(new File("color.csv"))
  writer.write("userId, firstName, lastName, phoneNumber, emailAddress\n")
  writer.write(csv)
  writer.close()


//  // Problem - 3
//  private val jsonData3 = Source.fromFile("/home/anand/Downloads/json/example6.json").mkString
//  private val donut = jsonData3.parseJson.convertTo[Donut]
//  private val writer3 = new PrintWriter(new File("donut.csv"))
//  writer3.write("id,type,name,ppu,batter_id,batter_type,topping_id,topping_type\n")
////  for (batter <- donut.batters) {
////    for (topping <- donut.topping) {
////      writer3.write(s"${donut.id},${donut.`type`},${donut.name},${donut.ppu},${batter},${topping.id},${topping.`type`}\n")
////    }
////  }
//  writer3.close()


   // Problem - 4












}
