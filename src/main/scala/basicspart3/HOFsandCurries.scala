package co.malle
package basicspart3

object HOFsandCurries extends App {


  def nTimes(f:Int=>Int,n:Int,x:Int):Int = {

    if(n<=0) x
    else nTimes(f,n-1,f(x))
  }

  def plusTwo:Int=>Int = x => x+2
  println(nTimes(plusTwo,3,10))

def nTimesBetter(f:Int=>Int,n:Int):(Int=>Int) = {

  if(n <=0 ) (x:Int) => x
  else (x:Int) => nTimesBetter(f,n-1)(f(x))

}

  val plus10 = nTimesBetter(plusTwo,10)
  println(plus10(3))

// fully curried

  def nTimesmuchbetter(f:Int=>Int)(n:Int):(Int=>Int) = {

    if(n<=0) (x:Int) => x
    else (x:Int) => nTimesmuchbetter(f)(n-1)(f(x))

  }

  val plus10much = nTimesmuchbetter(plusTwo)(_)
  val plus10much1 = plus10much(5)
  val plus10much2 = plus10much1(20)
  println(plus10much2)


}
