package co.malle
package collections

import scala.::
import scala.util.Random

object sequences extends App {
/*
  Sequences
  -> well defined order that can be indexed
  Ranges
  -> ranges also sequences

  Lists
  ->Lists is a linear sequence immutable linked list
  -> head tail and isEmpty methods are fast
  ->most operations on length and reverse


*/
  val aSeq = Seq(1,2,3,4,5)
  println(aSeq)
  println(aSeq.reverse)
  println(aSeq(2))
  println(aSeq.++(List(7,8)))

  val aRange: Seq[Int] = 1 to 10
  aRange.foreach(println)

val aList = 42 :: List(12,3)
  println(aList)

  //  +: is prepend infix operator and :+ is append operator

  val aList1 = 42 +: aList
  println(aList1)
  val aList2 = aList :+ 98
  println(aList2)


  val apple5: Seq[String] = List.fill(5)("malle")
  apple5.foreach(println)

  //arrays
  val array = Array(1,2,3,4,5)
  val array1 = Array.ofDim(3)
  array.foreach(println)
  array1.foreach(println)
  val arrayseq:Seq[Int]= array
  arrayseq.foreach(println)

  //vectors
  // vectors are the default implementation for the immutable collections
  // it offers constant read and write time for indexed operations
  val vector:Vector[Int] = Vector(1,2,3)
  println(vector)

  // vector vs lists

  /*
  List
  adv :it keeps reference to the tail
  dis:updating an element in the middle takes long time

  vector:
  adv: depth of the tree is small
  dis:needs to replace an entire 32 element chunk
   */

  val maxruns = 1000
  val maxcapacity = 10000000
  def getWriteTime(collection:Seq[Int]):Double = {
    val r = new Random
    val time = for(i <- 1 to maxruns) yield {
      var currenttime = System.nanoTime()
      collection.updated(r.nextInt(maxcapacity),0)
      currenttime - System.nanoTime()
    }

    ((time.sum)*1.0)/maxruns
  }
val numlist = (1 to maxcapacity).toList
  val numvec = (1 to maxcapacity).toVector
  println(getWriteTime(numvec))
  println(getWriteTime(numlist))

}
