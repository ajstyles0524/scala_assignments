package higherOrderFunctions

import scala.io.Source


// Name     City      Salary     Department
//
// Anand    HYD        60K       Engineering
// Mohan    BLR        40K       Human Resource
// Shyam    AMD        50K       Human Resource
// Dilip    HYD        100K      Manager
// Saif     BLR        90K       Engineering
// Akhil    AMD        60K       Engineering

object FileHandling extends App{

  val file = Source.fromFile("/home/anand/Downloads/handling_file.csv")

  // access each character
  // while(file.hasNext) println(file.next())

  // read content line by line
  // for(line <- file.getLines()) println(line)

  val data = file.getLines().drop(1).map(line => line.split(",").toList).toList
  data.foreach { line => println(line) }

  //private val validName = data.filter( ele => ele(2).take(2).toInt > 40).filter(ele => ele(3) == "Engineering" || "Human Resource")

  private val filteredData = data.filter { case (_ :: _ :: salary :: department :: _) =>
    salary.take(2).toInt > 40 && (department == "Engineering" || department == "Human Resource")
  }
  filteredData.foreach { case (name :: city :: _ :: _ :: _) =>
    println(s"$name from $city")
  }
  file.close()
}
