package test

import aproximacion.Lagrange
import numeros.Real;

/**
 * Created by IntelliJ IDEA.
 * User: jol
 * Date: 31-dic-2009
 * Time: 11:12:35
 * To change this template use File | Settings | File Templates.
 */

object TestInterpolacion{
  def main(args : Array[String]) : Unit = {
    val pol = Lagrange.lagrange[Real](List((1.0,2.0),(3.0,-20.0),(9.0,-10.0)));
    println(pol);
    println(pol(1.0)+" "+pol(3.0)+" "+pol(9.0));
  }
}