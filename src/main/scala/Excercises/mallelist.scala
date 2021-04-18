package co.malle
package Excercises

abstract class mallelist[+A] {
  def head:A
  def tail:mallelist[A]
  def isEmpty:Boolean
  def add[B >: A](element:B):mallelist[B]
  def map[B](malleTransformer: MalleTransformer[A,B]):mallelist[B]
  def filter(mallePredicate: MallePredicate[A]):mallelist[A]
  def ++[B >: A](list: mallelist[B]):mallelist[B]
 def flatMap[B](malleTransformer: MalleTransformer[A,mallelist[B]]):mallelist[B]
  def printElements:String
  override def toString: String = "[" +printElements +"]"
}


case object malleempty extends mallelist[Nothing]{
  override def head: Nothing = throw new NoSuchElementException
  override def tail: mallelist[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](element: B): mallelist[B] = new mallecons(element,malleempty)
  def printElements:String = ""
  override def map[B](malleTransformer: MalleTransformer[Nothing, B]): mallelist[B] = malleempty
  override def filter(mallePredicate: MallePredicate[Nothing]): mallelist[Nothing] = malleempty
  override def flatMap[B](malleTransformer: MalleTransformer[Nothing, mallelist[B]]): mallelist[B] = malleempty
  override def ++[B >: Nothing](list: mallelist[B]): mallelist[B] = list
}

case class mallecons[+A](h:A,t:mallelist[A]) extends mallelist[A]{
  override def head: A = h
  override def tail: mallelist[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](element: B): mallelist[B] = new mallecons(element,this)
  def printElements:String = {
    if(t.isEmpty) ""+h
    else  h + "" + t.printElements
  }
  override def map[B](malleTransformer: MalleTransformer[A, B]): mallelist[B] = {
    new mallecons[B](malleTransformer.tranform(h),t.map(malleTransformer))
  }
  override def filter(mallePredicate: MallePredicate[A]): mallelist[A] = {
    if(mallePredicate.test(h)) new mallecons[A](h,t.filter(mallePredicate))
    else t.filter(mallePredicate)
  }
  def ++[B >: A](list: mallelist[B]):mallelist[B] = new mallecons[B](h,t++list)
  override def flatMap[B](malleTransformer: MalleTransformer[A,mallelist[B]]): mallelist[B] = {
    malleTransformer.tranform(h) ++ t.flatMap(malleTransformer)
  }
}

trait MallePredicate[-A]{
  def test(element:A):Boolean
}
trait MalleTransformer[-A,B]{
  def tranform(element:A):B
}

object malletest extends App{
  val list = new mallecons[Int](1,new mallecons[Int](2,new mallecons[Int](3,new mallecons[Int](4,malleempty))))
  println(list.head.toString)
  println(list.tail.toString)
  println(list.isEmpty)
  println(list.add(30).toString)
  val map = list.map(new MalleTransformer[Int,Int] {
    override def tranform(element: Int): Int = element*2
  }).toString
  val filter = list.filter(new MallePredicate[Int] {
    override def test(element: Int): Boolean = element%2==0
  }).toString
  val fmap = list.flatMap(new MalleTransformer[Int,mallelist[Int]] {
    override def tranform(element: Int): mallelist[Int] = new mallecons[Int](element,new mallecons[Int](element+1,malleempty))
  }).toString
  println(map)
  println(fmap)
  println(filter)
}