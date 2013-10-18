package test

import numeros.Real
import estructuras.matriz.{AlmacenHueco, AlmacenDenso, Matriz}

/**
 * Created by IntelliJ IDEA.
 * User: jol
 * Date: 4/01/12
 * Time: 16:15
 * To change this template use File | Settings | File Templates.
 */

object TestFill {
  def main(args : Array[String]) : Unit = {
    val a = Matriz(AlmacenDenso[Real](50,50,()=>Real.randomRealPositivo(100,1000),250,System.currentTimeMillis()));
    val b = Matriz(AlmacenHueco[Real](50,50,a.toList :_*));
    println("ok");
    println(a.equals(b));
    println(a==b);
    println(a.toList==b.toList);
    
    val c = Matriz(AlmacenDenso[Real](4,4).fillSimetrica(()=>Real.randomRealPositivo(100,1000)));
    println(c);
  }

}