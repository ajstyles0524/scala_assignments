package higherOrderFunctions

object HOF extends App{

  // map, flatMap, reduce, filter, groupBy, partitionBy, fold foldLeft,foldRight


  // definition of HOF(passing function)
  private def functionExample(a: Int, f: Int => Int): Unit = println(f(a))
  private def multiplyBy2(i: Int) = i*2
  functionExample(25,multiplyBy2)


  // map - map is a function that transforms one collection into another collection by applying a function to each element.
  val collection = List(1, 3, 2, 5, 4, 7, 6)
  private def square(num:Int): Int = num*num
  val res = collection.map(square)
  private val res1= collection.map(x => if(x%2==0) x*x else x)
  println(res)
  println(res1)


  // flatMap - flatMap is identical to the map function, but the only difference is that in flatMap the inner grouping of an item is removed and a sequence is generated.
  val data = Seq("Anand KUMAR JAY", " Sanjay KUmar", "ALEX WALKER")
  private val res2 = data.map(_.toLowerCase()).flatten
  private val res3 = data.flatMap(_.toLowerCase())
  println(res2)
  println(res3)

  val list = List(2, 3, 4)
  private def f(x:Int) = List(x-1, x, x+1)
  private val r = list.map(x => f(x))
  private val res4 = list.flatMap(y => f(y))

  private val seq = Seq(8, 15, 22, 23, 24)
  private val res6 = seq.flatMap { s => Seq(s, s - 1)}
  private val res5 = seq flatMap { s =>if (s % 3 == 0) Seq(s) else Seq(-s)}
  println(r)
  println(res4)
  println(res5)
  println(res6)


  // filter - filter function takes a predicate and selects the elements from the data structure which satisfy the given predicate
  private val names = List("Anand", "Ram", "Shyam", "Kanta")
  private val res7 = names.filter(name => name.length >4)
  private val res8  = list.filter(x=>{x % 3 == 0})
  private val res9 = list.filter(_ < 3)
  println(res7)
  println(res8)
  println(res9)


  // reduce - reduce is a bit different function compare to the previous ones. It takes all the elements in a collection and combines them using a binary operation to produce a single value.
  private val res10 = collection.reduce((a, b) => a+b)
  private val res11 = data.reduce((a, b) => a+b)
  println(res10)
  println(res11)


  // fold - Like reduce fold also takes a binary operation which merges all the elements from the collection and returns a single value. The difference is that fold allows us to define an initial value
  private val res12 = collection.fold(0)((a, b) => a+b)
  private val res13 = data.fold("")((a, b) => a+ "-" +b)
  println(res12)
  println(res13)


  // scan - Scan function takes the binary operation as parameter and returns the value for each element in collection for that operation. It returns each iteration for that binary operator in the collection.
  private val res14 = collection.scan(0)((a, b) => a+b)
  private val res15 = data.scan("")((a, b) => a + b)
  println(res14)
  println(res15)

  // foldLeft -
  private val res16 = collection.foldLeft(0)((acc, i) => acc+i)
  private val res17 = data.foldLeft("")((acc, i) => acc+i)
  println(res16)
  println(res17)

  // foldRight -
  val res18 = collection.foldRight(0)((i,acc)=> i+acc)
  val res19 = data.foldRight("")((i,acc) => i+acc)

  // groupBy -
  val lis = List(1,2,3,4,1,2,5,6,0)
  private val res20 = lis.groupBy(lis)
  private val res21 = lis.groupBy(lis).mapValues(a => a.length>2)
  println(res20)
  println(res21)

  // partitionBy -

  // drop -

  // take -

  // takeRight -

  // dropWhile -







  val arr = Array(1,2,3,4,5)
  val result = Array(3,4,5,1,2)
  val n = 3
  val rotatedArr = arr.foldLeft(Array[Int]())((acc, x) => if (acc.length < arr.length - n) acc :+ x else Array(x) ++ acc)
  val rotatedLeft = arr.foldRight(Array[Int]())((x, acc) => if (acc.length < n) x +: acc else acc :+ x)
}

