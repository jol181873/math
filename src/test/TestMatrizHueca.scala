package test

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.util.Random
import numeros._
import estructuras.matriz.{AlmacenHueco, AlmacenDenso, CuasiMatriz}

object TestMatrizHueca {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    var rnd = new Random(System.currentTimeMillis);
    var b1 = CuasiMatriz(AlmacenHueco(2500,2500,()=>Real(rnd.nextDouble()),500,System.currentTimeMillis));
    var b2 = CuasiMatriz(AlmacenHueco(2500,2500,()=>Real(rnd.nextDouble()),500,System.currentTimeMillis));

    val t1=System.nanoTime();
    var b3=b1*b2;
    val t2=System.nanoTime();
    val b4=b1<*>b2;
    val t3=System.nanoTime();

    //println(b3);
    //println(b4);

    println("Tiempo multiplicacion normal    :" + (t2-t1)*1.0e-9);
    println("Tiempo multiplicacion multihilo :" + (t3-t2)*1.0e-9);
    println("¿Son iguales? "+ b3.equals(b4));
  }

}
