package dataStructure.stack
import scala.collection.mutable.Stack

class MinStack {

  private val st = new Stack[Int]()
  private var mini: Int = Int.MaxValue

  def pushVal(value: Int): Unit ={
    if(st.isEmpty){
      mini = value
      st.push(value)
    }
    else {
      if(value < mini){
        val temp = 2*value-mini
        st.push(temp)
        mini = value
      }
      else{
        st.push(value)
      }
    }
    println(value + " pushed into stack")
  }

  def popVal(): Unit ={
    if(st.isEmpty) println("Stack Under flow")
    else{
      val pop = st.pop()
      if (pop < mini) mini = 2 * mini-pop
    }
  }

  def topVal(): Int ={
    val value = st.top
    if(value < mini) mini
    else value
  }

  def getMin(): Int = mini

}

object minStack extends App{
  val stack = new MinStack
  stack.pushVal(-2)
  stack.pushVal(0)
  stack.pushVal(-3)
  println(stack.getMin())
  stack.popVal()
  println(stack.topVal())
  println(stack.getMin())
}