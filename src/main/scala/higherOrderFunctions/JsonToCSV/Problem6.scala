package higherOrderFunctions.JsonToCSV

import spray.json._
import java.io.{File, PrintWriter}
import scala.io.Source

case class Image(url: String, width: Int, height: Int)
case class Thumbnail(url: String, width: Int, height: Int)
case class Audio(id: String, `type`: String, name: String, image: Image, thumbnail: Thumbnail)


object VideoJsonProtocol extends DefaultJsonProtocol {
  implicit val videoFormat = jsonFormat5(Audio)
  implicit val imageFormat: RootJsonFormat[Image] = jsonFormat3(Image)
  implicit val thumbnailFormat: RootJsonFormat[Thumbnail] = jsonFormat3(Thumbnail)
}


object Problem6 extends App{
  import VideoJsonProtocol._
  private val jsonData4 = Source.fromFile("/home/anand/Downloads/json/example8.json").mkString
  private val data4 = jsonData4.parseJson.convertTo[Audio]
  val csvData = s"${data4.id},${data4.name},${data4.`type`},${data4.name},${data4.thumbnail.url},${data4.thumbnail.width},${data4.thumbnail.height}"
  println(csvData)

}
