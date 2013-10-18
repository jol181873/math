package test

import numeros.{EnteroModular, Entero}

/**
 * Created by IntelliJ IDEA.
 * User: jol
 * Date: 13-mar-2010
 * Time: 15:11:48
 * To change this template use File | Settings | File Templates.
 */

object TestBezout {
  def main(ars :Array[String]) :Unit = {
    val a = Entero(BigInt("19192827837374843736732"));
    val b = Entero(BigInt("38383838777"));
    println(a.mcd(b));
    println(a.coeficientesBezout(b));
    val c = EnteroModular(a,b);
    val d = c.inverso;
    println(c);
    println(d);
    println(c*d);
    println(d*c);
  }
}