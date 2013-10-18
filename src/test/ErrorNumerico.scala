package test

import numeros.Cuaternio

object ErrorNumerico {
  def main(args : Array[String]) : Unit = {
    val a =Cuaternio(9.246907095079991E+305,0.0,8.200986957150991E+307,5.019475925148729E+307)
    val b =Cuaternio(7.7969109358112E+307,1.3006284552598903E+307,8.988465674311579E+307,6.136431244973815E+307)
    val f =a.toMatrizReal;
    val g =b.toMatrizReal;
    println(a*b);
    //println(f*g);
    println(new Cuaternio(f*g))
    println(a*b==new Cuaternio(f*g));
  }
}
