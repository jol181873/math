package test

/*
 * TestFunciones.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import numeros._;
import estructuras.vector._;
import funciones._;

object TestFunciones {
  def main(args : Array[String]) : Unit = {
      var f = new Funcion(List("sen(pi/2)","x0+x2","sqrt(x3*x3)","sqrt(8*10+1)"));
      println(f.run("(1+1)/17"));
      //var vec = new VectorReal(List(1.0,-1.0,3.0,3.1415926));
      //println(f.calc(vec));
  }
}
