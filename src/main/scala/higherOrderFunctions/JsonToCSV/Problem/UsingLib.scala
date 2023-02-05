package higherOrderFunctions.JsonToCSV.Problem

object UsingLib extends App {


//  stripPrefix and stripSuffix to remove the square brackets at the beginning and end of the JSON data.
//
//   filter to only keep lines that contain the: character which represents key - value pairs in the JSON data
//
//  map to split each line into a key and a value
//  and then combine them into a string in the format key value.
//
//   grouped to group the strings into lists of three elements where each list represents the data
//   for one person
//
//  mkString to join all of the strings in the resulting list into a single string
//  with each element separated by a newline character.

  val jsonData =
    """[{
    "dob": 1879,
    "id": 1,
    "name": "albert einstein"
  }, {
    "dob": 1643,
    "id": 2,
    "name": "isaac newton"
  }, {
    "dob": 1867,
    "id": 3,
    "name": "marie curie"
  }, {
    "dob": 1564,
    "id": 4,
    "name": "galilée"
  }]"""

  private val csvHeader = "dob,id,name"
  val csvData = jsonData
    .stripPrefix("[")
    .stripSuffix("]")
    .split("\n")
    .filter(_.contains(":"))
    .map(line => {
      val Array(key, value) = line.split(":").map(_.trim)
      value.stripPrefix("\"").stripSuffix("\"")
    })
    .grouped(3)
    .map(values => values.mkString(""))
    .mkString("\n")

  println(csvHeader)
  println(csvData)


}
