package higherOrderFunctions


// a) Increase 10 % basic
// b) Increase 20% hra if age greater than 50
case class Salary(basic: Double, hra: Double, ta: Double)
case class Employee(id: Int, email: String, salary: Salary, age: Int)

// Spilt student list into four list as CS,IT,EC,ME. Student represented by :
case class Student(id:Int, name:String, age:Int, branch:String)

// Q -3
case class Customer(value: Int)
case class Consultant(portfolio: List[Customer])
case class Branch(consultants: List[Consultant])
case class Company(branches: List[Branch])


// Q - 4
sealed trait Expr
case class Number(i: Int) extends Expr
case class Sum(expr1: Expr, expr2: Expr) extends Expr
case class Subtract(expr1: Expr, expr2: Expr) extends Expr
case class Multiply(expr1: Expr, expr2: Expr) extends Expr

// Sort names by age; output should show name and age in scala
case class Person(name: String, age: Int)


// print the name,filter the age,total salary
case class Employee2(name: String, age: Int, salary: Int)

// add sick leave
case class Employee3(name: String, age: Int, salary: Double, leaves: List[String])


object CaseClass extends App{

  // Q - 1
  private def appraisal(employees: List[Employee]): List[Employee] = {
    employees.map {
      case Employee(id, email, Salary(basic, hra, ta), age) if age > 32 => Employee(id, email, Salary(1.1 * basic, 1.2 * hra, ta), age)
      case Employee(id, email, Salary(basic, hra, ta), age) => Employee(id, email, Salary(1.1 * basic, hra, ta), age)
    }
  }
  private val employees = List(Employee(101,"ABC@GMAIL.COM",Salary(50.00,30.00,20.00),33),
                       Employee(102,"DEF@GMAIL.COM",Salary(60.50,40.00,10.25),25),
                       Employee(103,"GRP@GMAIL.COM",Salary(90.25,30.00,8.00),35))
  private val appraisedEmployee = appraisal(employees)
  appraisedEmployee.foreach(ele => println(ele))

  // Q - 2
  private def spiltByBranch(list: List[Student]): (List[Student], List[Student], List[Student], List[Student]) = {
    val (cs, rest) = list.partition(_.branch == "CS")
    val (it, rest2) = rest.partition(_.branch == "IT")
    val (ec, me) = rest2.partition(_.branch == "EC")
    (cs, it, ec, me)
  }
  private def spiltByBranches(list: List[Student]): (List[Student], List[Student], List[Student], List[Student]) = {
    val grouped = list.groupBy(_.branch)
    (grouped.getOrElse("CS", List()), grouped.getOrElse("IT", List()), grouped.getOrElse("EC", List()), grouped.getOrElse("ME", List()))
  }
  private val students = List(Student(10,"Anand",24,"CS"), Student(11,"Sonu",25,"ME"),Student(12,"Aman",24,"IT"),Student(13,"Kamran",23,"EC"))
  private val groupedStudent = spiltByBranches(students)
  println(groupedStudent)


  // Q - 3
  def getCompanyValue(company: Company): Int = {
    val valuesList: List[Int] = for {
      branch <- company.branches
      consultant <- branch.consultants
      customer <- consultant.portfolio
    } yield customer.value
    valuesList.sum
  }

  // Q - 4
  def eval(expr: Expr): Int ={
    expr match{
      case Number(i) => i
      case Sum(expr1,expr2) => eval(expr1) + eval(expr2)
      case Subtract(expr1, expr2) => eval(expr1) - eval(expr2)
      case Multiply(expr1, expr2) => eval(expr1) * eval(expr2)
    }
  }
  val expr = eval(Sum(Number(5),Number(10)))
  println(expr)


  // Q - 5
  private def sortByAge(person: List[Person]): Unit ={
     val sorted = person.sortBy(_.age)
    sorted.foreach(ele => println(s"${ele.name}- ${ele.age}"))
  }
  private val persons = List(Person("Anand",20),Person("Aman",18),Person("Dilip",34))
  sortByAge(persons)

  // Q - 6
  private def employee(employees: List[Employee2]): Unit ={
    val empName = employees.map(_.name)
    println(empName)
    val ageGreaterThan30 = employees.filter(_.age > 30)
    println(ageGreaterThan30)
    val totalSalary = employees.foldLeft(0.0)((acc,i) => acc + i.salary)
    println(totalSalary)
  }
  private val employee = List(
    Employee2("John Doe", 30, 500),
    Employee2("Jane Smith", 35, 60000),
    Employee2("Bob Johnson", 25, 45000),
    Employee2("Emily Davis", 28, 55000))
  employee(employee)


  // Q - 7
  private def modifyEmployee(employees: List[Employee3]): Unit ={
    val employeesOnSickLeave = employees.filter(employee => employee.leaves.contains("Sick Leave")).map(e => e.name)
    println(employeesOnSickLeave)
    val totalLeaves = employees.map(_.leaves.length).sum
    println(totalLeaves)

    for (employee <- employees) {
      println(s"Employee Name: ${employee.name}")
      println(s"Employee Salary Records: ${employee.salary}")
    }
  }

  private def updateLeaves(employees: List[Employee3]): List[Employee3] = {
    employees.map {
      case e if e.age > 29 => e.copy(salary = 559099)
      case e if e.name == "Emily Davis" => e.copy(leaves = e.leaves :+ "Maternity Leave")
      case e => e
    }
  }
  private val employee2 = List(
    Employee3("John Doe", 30, 50000.0, List("Sick Leave", "Vacation Leave")),
    Employee3("Jane Smith", 35, 60000.0, List("Sick Leave")),
    Employee3("Bob Johnson", 25, 45000.0, List("Vacation Leave")),
    Employee3("Emily Davis", 28, 55000.0, List()))
    private val emp2 = updateLeaves(employee2)
    println(emp2)
    modifyEmployee(employee2)
}
