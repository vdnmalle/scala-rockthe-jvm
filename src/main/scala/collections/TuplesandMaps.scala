package co.malle
package collections

object TuplesandMaps extends App {

  /*
  Tuples - basically a finite ordered lists

   */


  // Maps - Keys -> Values

  val aMap:Map[String,Int] = Map()
  val phoenbook = Map(("malle","9094617194"), "Malle" -> "98783344").withDefaultValue(-1)
  val phlowercase = phoenbook.map(pair => pair._1.toLowerCase() -> pair._2)
  println(phlowercase)
  println(phoenbook)

}
