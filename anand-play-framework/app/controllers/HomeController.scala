package controllers

import javax.inject._
import play.api._
import play.api.mvc._

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }


  // 1. JSON Notation

  // In Scala, JSON notation is used to represent data in a format
  // that is easy for humans to read and machines to parse. JSON stands for JavaScript Object Notation
  // ....
  // JSON notation uses a key-value format, where keys are strings and
  // values can be strings, numbers, booleans, arrays, or other JSON objects.
  // ....
  // In scala, There are several libraries that can be used to work with JSON including json4s, circe, and upickle.
  // Each of these libraries provides a way to work with JSON notation in Scala, including parsing JSON strings, generating JSON strings, and working with JSON data in a type-safe manner.
  // ....
  // Additionally, the Play Framework provides its own JSON library, play.api.libs.json,
  // which provides a similar set of functionality for working with JSON notation in Scala.

  // Here's an example of creating a JSON object using the JsObject class:
  // The Json.obj method creates a new JsObject instance with the given fields. Each field is defined as a tuple of a string (representing the field name) and a value.

  import play.api.libs.json._

  val json = Json.obj(
    "name" -> "John Doe",
    "age" -> 30
  )

  //2. generating json string

  // The play.api.libs.json package provides a set of classes and methods for working with JSON data
  // The Json.obj method creates a new JsObject instance with the given fields. Each field is defined as a tuple of a string (representing the field name) and a value.
  // And the jsonString variable will contain the json in string format. You can also use Json.stringify to get the json string from json object.
  // This creates a JSON object with the following structure:

  import play.api.libs.json._

  val json = Json.obj(
    "name" -> "John",
    "age" -> 30,
    "address" -> Json.obj(
      "street" -> "Main St.",
      "city" -> "Anytown",
      "state" -> "CA"
    )
  )
  val jsonString = json.toString() {
    "name": "John"
    ,
    "age": 30
    ,
    "address": {
      "street": "Main St.",
      "city": "Anytown",
      "state": "CA"
    }
  }



  // 3.parsing json from string

  // you can use the Json.parse method from the play.api.libs.json package to parse JSON from a string in Scala.
  // This method returns a JsValue object, which represents the parsed JSON and can be used to extract values from the JSON
  // In this example, the as method is used to extract the value of a specific field from the JsValue object and convert it to a specific type.

  import play.api.libs.json._

  val jsonString = """{"name":"John","age":30,"city":"New York"}"""
  val json: JsValue = Json.parse(jsonString)
  val name = (json \ "name").as[String]
  val age = (json \ "age").as[Int]
  val city = (json \ "city").as[String]
  println(name)

  // You can also use the validate method to extract values from json based on their types and also if their presence is mandatory or optional,
  val jsonString = """{"name":"John","age":30,"city":"New York"}"""
  val json = Json.parse(jsonString)

  case class Person(name: String, age: Int)

  val result: JsResult[Person] = json.validate[Person]
  val person = json.as[Person] // Also, you can use the as method to convert the JsValue to a case class,


  //4. converting string into case classes

  // In order to convert a string to a case class in Scala using the Play JSON library,
  // you can use the Json.parse method to parse the string into a JsValue, and then use the as method to convert it to the case class.
  // In this example, we first import the necessary libraries, then define a case class Person with the fields name and age.
  // We also define an implicit Format for the Person case class using Json.format[Person].
  // Next, we create a string that represents a JSON object, parse that string into a JsValue using Json.parse(jsonString),
  // and then use the as method to convert the JsValue into an instance of the Person case class.

  import play.api.libs.json._

  case class Person(name: String, age: Int)

  object Person {
    implicit val personFormat: Format[Person] = Json.format[Person]
  }

  // implicit val personFormat: Format[Person] = Json.format[Person]
  val jsonString = """{"name": "John Doe", "age": 30}"""
  val json: JsValue = Json.parse(jsonString)
  val person: Person = json.as[Person]


  // Or this we can
  // In this example, the jsonString variable holds a JSON representation of an object,
  // which is parsed into a JsValue using the Json.parse method. The json.validate[Person] method is used to convert the JsValue to a Person case class. The result is then matched with JsSuccess and JsError to handle the result.

  import play.api.libs.json._
  case class Person(name: String, age: Int)
  implicit val personFormat: Format[Person] = Json.format[Person]
  object Main extends App {
    val jsonString = """{ "name": "John", "age": 30 }"""
    val json = Json.parse(jsonString)
    val result: JsResult[Person] = json.validate[Person]
    result match {
      case JsSuccess(person, _) => println(person)
      case JsError(errors) => println(errors)
    }
  }

  // 5. Convert JSON data to Scala object

  import play.api.libs.json._
  case class Person(name: String, age: Int)
  implicit val personFormat: Format[Person] = Json.format[Person]
  val jsonString = """{ "name": "John", "age": 30 }"""
  val json = Json.parse(jsonString)
  val result: JsResult[Person] = json.validate[Person]
  result match {
    case JsSuccess(person, _) => println(person) // prints Person(John,30)
    case JsError(errors) => println(errors)
  }

  // 6. Convert Scala object to JSON data

   // you can convert a Scala object to JSON data using the Json.toJson method
   // In this example, a case class Person is defined with name and age fields. Then, an implicit Format object is defined for the Person case class using Json.format[Person].
   // A new instance of Person is created with name as John and age as 30.
   //The Json.toJson method is used to convert the person object to a JSON string. It takes an implicit Format object as an argument, so it knows how to convert the case class to JSON data.
   //The result of the Json.toJson method is a JsValue object, which can be used in the application.

  import play.api.libs.json._
  case class Person(name: String, age: Int)
  implicit val personFormat: Format[Person] = Json.format[Person]
  val person = Person("John", 30)
  val json = Json.toJson(person)
  println(json) // Output: {"name":"John","age":30}




   // 7. Convert a simple Map [String, String] to JSON

  import play.api.libs.json._
  val data: Map[String, String] = Map("name" -> "John", "age" -> "30")
  val json = Json.toJson(data)
  println(json) // Output: {"name":"John","age":"30"}
  // Alternatively, you can use Json.obj method to convert the map to Json
  val json = Json.obj("name" -> "John", "age" -> "30")



  // 8. From JSON data (file or string) get a list of unique country names
  //    Ex: [{ "country" : "India” , "State" : "AP },{ "country" : "India” , "State" : "TN },{ "country" : "USA“, "State" : "Virginia” }]       -> [“India”, “USA”]

  //