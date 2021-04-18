
package co.malle

object test extends App {

  def nTimes(f : Int =>Int,n:Int,x:Int):Int ={

    if(n <=0) x
    else nTimes(f,n-1,f(x))
  }

  def plusOne:Int =>Int = x => x+1
  println(nTimes(plusOne,10,1))
  val l = List(1,2,3)



  val network = Map("a" -> List("malle", "ranjani", "lakshmi", "soma", "Jyothi"))
    val nnetwork =  network.+("krithvik")
  println(nnetwork)


}

