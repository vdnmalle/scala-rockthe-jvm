package co.malle
package basicspart3

object functionalexcercises extends App {

  val stringConcat:(String,String)=>String = new Function2[String,String,String] {
    override def apply(v1: String, v2: String): String = v1.concat(v2
    )
  }

  println(stringConcat("malle","is a good guy"))


  // excercise 3

  val superadder: Int => Int => Int = new Function[Int,Function1[Int,Int]] {
    override def apply(v1: Int): Int => Int = new Function[Int,Int] {
      override def apply(v2: Int): Int = v1+v2
    }
  }

  val superadder2 :Int =>(Int => Int) = (x:Int) => (y:Int) => x+y

  val adder = superadder(3)(4)  // curried function

  val doubler:Int=>Int = (x:Int) => x+2
  val JustDoSomething: () => Int = () => 5
  val mul :Int=>Int = _*4

  def tocurry(f:(Int,Int) => Int):(Int=>Int=>Int) = x => y => f(x,y)

}
