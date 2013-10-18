package test

import estructuras.vector._;
import numeros._;

object Cas {
  def main(args : Array[String]) : Unit = {
    val lst=List("y*cos(x)+x*sin(y)","x^2+y");
    val f=new cas.Funcion(lst);

    val derivada=f.fnc(0).D("x");
    print(f.fnc(0).toString+"  ");
    println(derivada.toString)
    for(i<-0 to 25) for(j<-0 to 25) {
      val vector = Kn[Real](i*2.0/25,j*Math.PI/25);
      println(f.fnc(0)(vector)+"   "+derivada(vector));
    }

  }
}
