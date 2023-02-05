package higherOrderFunctions.CSVToJson

object Problem2 extends App{

  val csvData =
    """dob,id,name
  1879,1,albert einstein
  1643,2,isaac newton
  1867,3,marie curie
  1564,4,galilée"""

  val jsonData = csvData
    .stripPrefix("dob,id,name\n")
    .split("\n")
    .map(line => {
      val Array(dob, id, name) = line.split(",")
      s"""{"dob":$dob,"id":$id,"name":"$name"}"""
    })
    .mkString("[\n", ",\n", "\n]")
  println(jsonData)
}
