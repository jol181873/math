package test

import cas._;

object TestCas2 {
  def main(args : Array[String]) : Unit = {
    var f=new Funcion(List("5*sin(x)*6*name+cos(x)*4"));
    f.fnc.foreach(println)
    var d=f.fnc.map(_.D("x"));
    d.foreach(println);
  }
}
