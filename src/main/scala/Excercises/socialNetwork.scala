package co.malle
package Excercises

object socialNetwork extends App {

  def facebook(network:Map[String,Set[String]],person:String):Map[String,Set[String]]  = {

    network + (person -> Set())
  }

  def friend(network:Map[String,Set[String]],a:String,b:String):Map[String,Set[String]] = {
    val friendsA =  network(a)
    val friendsB = network(b)
    network + (a -> (friendsA + b)) + (b -> (friendsB + a))
  }

  def unfriend(network:Map[String,Set[String]],a:String,b:String):Map[String,Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)
    network + (a -> (friendsA - b)) + (b -> (friendsB - a))
  }






}
