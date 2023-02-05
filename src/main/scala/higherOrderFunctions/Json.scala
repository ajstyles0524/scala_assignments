package higherOrderFunctions

//import spray.json._
//
//case class Person(name: String, age: Int)
//private object MyJsonProtocol extends DefaultJsonProtocol {
//  implicit val personFormat: JsonFormat[Person] = jsonFormat2(Person)
//}
//
//
//case class ShortName(name: String, `type`: String)
//case class State(name: String, capital: String, official_language: String, other_names: List[ShortName])
//case class CountryData(country: String, states: List[State])
//
//object JsonProtocol extends DefaultJsonProtocol {
//    implicit val shortNameFormat: JsonFormat[ShortName] = jsonFormat2(ShortName)
//    implicit val stateFormat: JsonFormat[State] = jsonFormat4(State)
//    implicit val countryDataFormat: JsonFormat[CountryData] = jsonFormat2(CountryData)
//}
//
//
//object Json extends App{
//
//  // JSON is easy for humans to read and write and easy for machines to parse and generate
//  // JSON stands for JavaScript Object Notation
//  // JSON is often used when data is sent from a server to a web page or vice versa
//  // JSON data is written as name/value pairs, just like JavaScript object properties.
//  // JSON objects are written inside curly braces.  {"firstName":"John", "lastName":"Doe"}
//
//  // JSON is lightweight and easy to read and write, which makes it a popular choice for data transfer.
//  // It is also less verbose and less strict than XML, Overall, JSON is widely used because of its simplicity and flexibility,
//  // which makes it a great choice for data interchange
//
//  // spray-json is a lightweight, clean and efficient JSON implementation in Scala
//  // Spray-JSON provides a simple and efficient way to convert between Scala objects and JSON data, and
//  // it also includes support for automatic serialization and deserialization of case classes, as well as
//  // support for JSON arrays and optional fields.
//
//  // In scala, There are several libraries that can be used to work with JSON including json4s, circe, and upickle.
//  // Each of these libraries provides a way to work with JSON notation in Scala, including parsing JSON strings, generating JSON strings, and working with JSON data in a type-safe manner.
//
//
//  // 1. JSON Notation  or converting string into case classes or Convert JSON data to Scala object
//
//  // Spray-JSON uses a notation called "jsonFormat" to define how to serialize and deserialize case classes.
//  // The jsonFormat notation is used to define a "Format" object that describes how to convert between a case class and its JSON representation
//  // Here is an example of using jsonFormat to define a format for a simple case class called "Person":
//  // In this example, jsonFormat2 is used to define a format for a case class with two fields, "name" and "age".
//  // The jsonFormat notation takes a constructor function as its argument,
//  // in this case the apply method of the case class. This tells Spray-JSON how to convert between the case class and its JSON representation.
//  // Once the format is defined, it can be used to convert between a case class and its JSON representation using the "toJson" and "fromJson" methods:
//  // the parseJson method is used to parse the JSON string into a JsValue object, which is then passed to the convertTo[T] method.
//    // The convertTo[T] method uses the format defined in the jsonFormat2 notation to convert the JSON data to an instance of the Person case class.
//
//    import MyJsonProtocol._
//    val jsonString = """{"name":"Alice","age":25}"""
//    private val jsonAst = jsonString.parseJson
//    val person = jsonAst.convertTo[Person]
//    println(person)
//
//
//    // 2. parsing json from String
//    //  the parseJson method is used to parse the JSON string into a JsValue object
//    private val jsonS = """{"name":"Alice","age":25}"""
//    val json = jsonS.parseJson
//    println(json)
//    // You can also parse json from file using the io.Source.fromFile method, which reads the content of a file and returns it as a String.
//    //val file = new File("path/to/your/file.json")
//    //val jsonString = io.Source.fromFile(file).mkString
//    //val jsonAst = jsonString.parseJson
//
//
//    // 3. generating json string
//    private val json3 = JsObject("name" -> JsString("Alice"), "age" -> JsNumber(25))
//    private val jsonString3 = json3.compactPrint
//    println(jsonString3)
//
//    // 4. Convert Scala object to JSON data
//    // case class Person and MyJsonProtocol already define in start
//    import MyJsonProtocol._
//    private val person2 = Person("Alice", 25)
//    private val json4 = person2.toJson
//    private val jsonString4 = json4.compactPrint
//    println(jsonString4)
//
//    //val file = new File("path/to/your/file.json")
//    //val writer = new PrintWriter(file)
//    //writer.write(jsonString)
//    //writer.close()
//
//
//    // 5. Convert a simple Map [String, String] to JSON
//    val map = Map("name" -> "Alice", "age" -> "25")
//    private val json5 = map.toJson
//    private val jsonString5 = json5.compactPrint
//    println(jsonString5)
//
//
//
//    // 6. From JSON data (file or string) get a list of unique country names
//    //Ex: [{ "country" : "India” , "State" : "AP },{ "country" : "India” , "State" : "TN },{ "country" : "USA“, "State" : "Virginia” }]       -> [“India”, “USA”]
//
//    private val jsonString6 = """[{"country" : "India", "State" : "AP"},{"country" : "India", "State" : "TN"},{"country" : "USA","State" : "Virginia"}]"""
//    private val json6 = jsonString6.parseJson
//    private val countries = json6.convertTo[List[Map[String, String]]].map(_("country")).distinct
//    println(countries)
//    //import scala.io.Source
//    //val filePath = "path/to/json/file.json"
//    //val jsonString = Source.fromFile(filePath).mkString
//    //val jsonAst = jsonString.parseJson
//    //val countries = jsonAst.convertTo[List[Map[String, String]]].map(_("country")).toSet.toList
//    //println(countries)
//
//
//     // 7. From the following JSON file only printout the short names of the states that start with a vowel:
//    private val json7 =
//        """
//    [
//        {
//            "country": "India",
//            "states": [
//                {
//                    "name": "Andhra Pradesh",
//                    "capital": "Amaravathi",
//                    "official_language": "Telugu",
//                    "other_names": [
//                        {
//                            "name": "AP",
//                            "Type": "SHORT_NAME"
//                        }
//                    ]
//                },
//                {
//                    "name": "Telangana",
//                    "capital": "Hyderabad",
//                    "official_language": "Telugu",
//                    "other_names": [
//                        {
//                            "name": "TN",
//                            "Type": "SHORT_NAME"
//                        }
//                    ]
//                }
//            ]
//        }
//    ]
//    """
//    private val jsonAst7 = json7.parseJson
//    private val shortNames = jsonAst7.asInstanceOf[JsArray].elements
//      .flatMap(_.asJsObject.getFields("states"))
//      .flatMap(_.asInstanceOf[JsArray].elements)
//      .flatMap(_.asJsObject.getFields("other_names"))
//      .flatMap(_.asInstanceOf[JsArray].elements)
//      .filter(name => List('A', 'E', 'I', 'O', 'U').contains(name.asJsObject.getFields("name").head.asInstanceOf[JsString].value.head.toUpper))
//      .map(name => name.asJsObject.getFields("name").head.asInstanceOf[JsString].value)
//    println(shortNames)
//
//
//    // we can do using case class also
////    import JsonProtocol._
////    private val country = json7.parseJson.convertTo[List[CountryData]]
////    private val shortName = country.flatMap(_.states)
////      .flatMap(_.other_names)
////      .filter(name => List('A', 'E', 'I', 'O', 'U').contains(name.name.head.toUpper))
////      .map(_.name)
////    println(shortName)
//}
