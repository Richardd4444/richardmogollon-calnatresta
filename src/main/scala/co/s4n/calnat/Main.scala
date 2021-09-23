package co.s4n.calnat

import scala.io.StdIn

object Main extends App {
  def leerInt(prompt:String):Int = {
    val s = StdIn.readLine(prompt)
    s.toInt
  }
  def esCero(nat:Nat) = nat match {
    case Cero() => true
    case Suc(nat) => false
  }
  def esMayorIgual(nat1:Nat, nat2:Nat):Boolean = nat1 match {
    case Cero() => nat2 match {
      case Cero() => true
      case _      => false
    }
      case Suc(pnat) => nat2 match {
        case Cero() => true
        case Suc(snat) => esMayorIgual(pnat,snat)
      }
  }
  def conIntANat(i:Int):Nat = {
    if (i<=0) scala.math.abs(i)
    else i
  }
  def strToInts(str:String) = str.split(" ").filter(_!="").map(_.toInt)
  def readTS(prompt:String) : (Int) = {
    val s = StdIn.readLine(prompt)
    val ints = strToInts(s)
    (ints(0))
  }
  def imprimirNat(nat:Nat):String = nat match{
    case -1 => "No se puede realizar"
     case _ =>  {
      val d = nat%10
      val c = (d + '0').toChar
      imprimirNat(nat/10):+c
  }
  def restaNat(nat1:Nat, nat2:Nat):Nat = (nat1,nat2) match {
    case (nat1,0) => imprimirNat(nat1)
    case (nat1,nat2) => {
      if (a>b) restaNat(nat1-1, nat2-1)
      else imprimirNat(-1)
  }
  // Main
  val (a) = readTS("Leer primer entero ")
  val (b) = readTs("leer segundo entero ")
  print(imprimirNat(restaNat(conIntANat(a),conIntANat(b))))
}
