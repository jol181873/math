package test

import estructuras._
import matriz.Matriz
import cas._
import matriz.AlmacenDenso

object TestCas3Matriz {
  def main(args : Array[String]) : Unit = {
    var a = Matriz(AlmacenDenso[Expr](2,2,"a","b","c","d"));
    var b = Matriz(AlmacenDenso[Expr](2,2,2.0,3.0,4.0,5.0));
    println(a);
    println(b);
    println(a+b);
    println(a*b);
    println(a.det);
    println(a.inversaDet);
    //println(a);
    println(a.inversa);
    //println(a);
    println(a*a.inversaDet)
  }
}
