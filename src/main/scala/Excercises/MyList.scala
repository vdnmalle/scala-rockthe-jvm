package co.malle
package Excercises

abstract class MyList[+A] {

  def head:A
  def tail:MyList[A]
  def isEmpty:Boolean
  def add[B >: A](element:B):MyList[B]
  def printElements:String
  override def toString: String = "[" + printElements + "]"
  def map[B](transformer: (A) => B):MyList[B]
  def filter(predicate:A => Boolean):MyList[A]
  def ++[B >: A](list:MyList[B]):MyList[B]
  def flatMap[B](transformer: (A) => MyList[B]):MyList[B]

  //HOFS
  def foreach(f:A=>Unit):Unit
}


case object empty extends MyList[Nothing]{
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = false
  override def add[B >:Nothing](element:B): MyList[B] = new cons(element,empty)
  def printElements = ""
  def map[B](transformer:Nothing=>B):MyList[B] = empty
  def filter(predicate:Nothing => Boolean):MyList[Nothing] = empty
  def ++[B >: Nothing](list:MyList[B]):MyList[B] = list
  def flatMap[B](transformer: Nothing=>MyList[B]):MyList[B] = empty
  //HOFS
  override def foreach(f: Nothing => Unit): Unit = ()
}

case class cons[+A](h:A,t:MyList[A]) extends MyList[A]{
  override def head: A = h
  override def tail: MyList[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](element:B): MyList[B] = new cons(element,this)
  def printElements:String = {
    if (t.isEmpty) "" + h
  else h + " " + t.printElements
}

  /*
  [1,2,3].map(_*2)

  1.
   */


  def map[B](transformer: A=>B):MyList[B] =

    new cons[B](transformer.apply(h),t.map(transformer))


  def filter(predicate:A=>Boolean):MyList[A] = {

    if(predicate.apply(h)) new cons[A](h,t.filter(predicate))
    else t.filter(predicate)
  }

  def ++[B >: A](list:MyList[B]):MyList[B] = new cons[B](h,t++list)
  def flatMap[B](transformer:A=>MyList[B]):MyList[B] = {
    (transformer.apply(h) ++ t.flatMap(transformer))

  }

  //HOFS

  override def foreach(f: A => Unit): Unit = {

    f(h)
    t.foreach(f)
  }
}

/*trait MyPredicate[-T]{

  def test(element:T):Boolean
}

trait MyTransformer[-A,B]{

  def transform(element:A):B
}*/

object test extends App{

  val listi = new cons(1,new cons(2,new cons(3,empty)))
  val lists = new cons[String]("malle",new cons("learning",new cons[String]("scala",empty)))
//  println(list.add(20))
//  println(list.head)
 // println(list.tail)
  println(listi.toString)
  println(lists.toString)
 // println(list.printElements)
  println(listi.map((element: Int) => element * 2).toString)
  println(listi.filter((element: Int) => element % 3 == 0).toString)
  println(listi.flatMap((element: Int) => new cons[Int](element, new cons[Int](element + 1, empty))).toString)

  //HOFS
  listi.foreach(println)
  lists.foreach(println)
}


