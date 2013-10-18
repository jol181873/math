package test

import numeros.Cuaternio

object TestCuaternio {
  def main(args : Array[String]) : Unit = {
    val a=Cuaternio(1.4138545665366735E307,-7.479797511677881E307,8.988465674311579E307,8.988465674311579E307);
    println(a);
    println(a.inverso)
    println(a.conjugado)
    println(a.sqrnorma)
    println(a.inverso*a);
    //println((4/1.4138545665366735E307/100e120))
    
    val b=Cuaternio(10,5,2,7);
    println("================================");
    println(b);
    println(b.inverso)
    println(b.conjugado)
    println(b.sqrnorma)
    println(b.inverso*b);
  }
}
