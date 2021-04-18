package co.malle
package Recursion

import scala.annotation.tailrec

object factorial extends App {

  // this is recursion
  def fact(n: Int): Int = {

    if (n == 0) 1
    else n * fact(n - 1)

  }

  //now factorial tail recursion

  @tailrec
  def facttail(n: Int, acc: Int): Int = {

    if (n == 0) acc
    else facttail(n - 1, n * acc)

  }

  println(fact(5))
  println(facttail(5, 1))


  // concatinating a String n Times using tail recursion

  @tailrec
  def concatStrings(s: String, acc: String, n: Int): String = {

    if (n == 0) return acc
    else concatStrings(s, acc.concat(s), n - 1)
  }

  println(concatStrings("malle", " ", 7))


  //IsPrime normal

  def isPrime(n:Int):Boolean = {
    val acc1 = n/2
    var result = true
    if(n==0 || n==1) false
    else {

      for(i <- 2 to acc1 by 2){
        result =  if(n%i==0) false
        else true
      }

      result
    }

  }

println(isPrime(24))


  //isPrime tail recursion

  def isPrimet(n:Int): Boolean ={

    def primetest(t:Int,isStillPrime:Boolean): Boolean ={

      if(!isStillPrime) false
      else if(t <=1) true
      else  primetest(t-1, n%t!=0 && isStillPrime)

    }
       primetest(n/2,true)
  }

 println(isPrimet(20))
  println(isPrimet(33))
  println(isPrimet(23))
  println(isPrimet(403))

}
