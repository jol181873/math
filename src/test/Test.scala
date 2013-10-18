package test

;

import numeros._;
import Complejo._;
import estructuras._;
import estructuras.vector._;
import estructuras.matriz._;
import utilidades._;
import edos._;

object Test {
  def main(args : Array[String]) : Unit = {
      var a = Kn[Real](3,1,-3);
      var b = Kn[Real](-5,3,2);

      println(a);
      println(b);
      println(a+b);
      println(a-b);
      println(a*b);

      println("================================");
      var p1 = Polinomio[Real](3,1);
      var q1 = Polinomio[Real](5,3,2,5,-6);

      println("p="+p1);
      println("q="+q1);
      println("p+q="+(p1+q1));
      println("q+p="+(q1+p1));
      println("p-q="+(p1-q1));
      println("q-p="+(q1-p1));
      println("p*q="+(p1*q1));
      println("q/p="+(q1/p1));
      println("q%p="+(q1%p1));
      println("mcd(q,p)="+q1.mcd(p1));
      println("mcd(p.q)="+p1.mcd(q1));

      println(Polinomio.ciclotomicoC(6));
      println(Polinomio.ciclotomicoQ(6));

      println("================================");
      var y :Racional[Entero] = Racional(-15,4);
      //println(x);
      println(y);

      println("================================");
      var v1 = R3(1.0,-2.0,3.0);
      var v2 = R3(-3.0,1.0,-4.0);
      println(v1.dim);
      println(v1);
      println(v2);
      println(v1+v2);
      println(v1*v2);
      //println(v1*3.0);
      println(v1**v2);
      println(v1*(v1**v2));
      println(v2*(v1**v2));
      println("Norma 1 de v1: "+v1.norma1);
      println("Norma 2 de v1: "+v1.norma2);
      println("Norma inf    : "+v1.normaInfinito);
      println("Norma 1 de v2: "+v2.norma1);
      println("Norma 2 de v2: "+v2.norma2);
      println("Norma inf    : "+v2.normaInfinito);
      //var t :Kn[Entero]=new Kn[numeros.Real](1,2,3);

//      println("================================");
//      var m = new MatrizReal(List(List(1.0,2.0),List(-1.0,2.0)));
//      var n = new MatrizReal(List(List(2.0,-1.0,4.0,3.0),List(-1.0,1.0,-3.0,-1.0),List(4.0,2.0,-1.0,2.0),List(-1.0,-2.0,3.0,-2.0)));
//      println(m);
//      println(m.det);
//      println(m.polChar);
//      println(m.inversa);
//      println(m*m.inversa);
//      var z=n.QR;
//      var q=z._1;
//      var r=z._2;
//      println(n);
//      println("Q=");
//      println(q);
//      println("R=");
//      println(r);
//      println("Q es ortogonal?");
//      println(q * !q);
//      println("Q*R=");
//      println(q*r);
//      println("Valores propios 5 iteraciones: "+n.valores_propios(5).toString);
//      println("Valores propios 10 iteraciones: "+n.valores_propios(10).toString);
//
//      println("================================");
//      var n2 = new MatrizReal(List(List(2.0,-1.0,4.0,3.0),List(-1.0,2.0,-3.0,1.0),List(4.0,-3.0,2.0,1.0),List(3.0,1.0,1.0,3.0)));
//      var z2=n2.QR;
//      var q2=z2._1;
//      var r2=z2._2;
//      println(n2);
//      println(n2.polChar);
//      println("Q=");
//      println(q2);
//      println("R=");
//      println(r2);
//      println("Q es ortogonal?");
//      println(q2 * !q2);
//      println("Q*R=");
//      println(q2*r2);
//      println("Valores propios 5 iteraciones: "+n2.valores_propios(5).toString);
//      println("Valores propios 10 iteraciones: "+n2.valores_propios(10).toString);
//      println("================================");
//      var rk1 =new RungeKutta("./dp54/dp54.rk");
//      println(rk1.a);
//      println(rk1.b);
//      println(rk1.be);
//      println(rk1.c);
      val frac = Racional[Entero](1,3);
      println(frac.Re);
  }
}
