package test

/*
 * Test3.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import estructuras._;
import numeros._;
import numeros.Real._;
import numeros.Cuaternio._;
import matriz._;
import utilidades.Consola;

object Test3 {
      def main(args : Array[String]) : Unit = {
            //var frame :JFrame= new JFrame("Hola cuentas");
            //var texarea=new JTextArea();
            //frame.getContentPane.add(texarea);
            //var m = Matriz[numeros.Real](2,2,numeros.Real.cero,1.0,2.0,-1.0,2.0);
            /*var n = XXMatriz[Racional[Entero]](2,2,Racional(Polinomio[Racional[Entero]](2,-1),Polinomio[Racional[Entero]](-1,1,-3)),
                                                       Racional(Polinomio[Racional[Entero]](4,2),Polinomio[Racional[Entero]](-1,-2)),
                                                       Racional(Polinomio[Racional[Entero]](2,-2),Polinomio[Racional[Entero]](-3,1)),
                                                       Racional(Polinomio[Racional[Entero]](-4,3),Polinomio[Racional[Entero]](-5,-2,1)));*/
            /*println(n);
            var (a,b) = n.escalona;
            println(a);
            var a=n.inversa;
            println(a);
            println(n*a);
            println(n.det);
            println(n.det2);*/
            var t = Polinomio[Racional[Entero]](+4,+6);
            var t2= Polinomio[Racional[Entero]](-8,-2,2);
            var r =Polinomio[Real](2.0,-1.5,3.4,-2.0);
            var f :Racional[Polinomio[Racional[Entero]]]= new Racional(t,t2);
            println(t);
            println(t2);
            //println(n);
            /*var mcd=t.mcd(t2);
            println(mcd);
            println(t/mcd);
            println(t2/mcd);
            var y = Racional(t,t2);
            print(texarea);
            frame.setVisible(true);
            */
            /*Consola.xprint_pol_rac(t);
            Consola.xprint("   ");
            Consola.xprint_pol_rac(t2);
            Consola.gotoxy(0,7);
            Consola.xprint_rac_pol(f);
            Consola.imprime(50,15);*/
        //f.xprint(Consola);
        //r.xprint(Consola);

        /*n.xprint(Consola);
        val q :Cuaternio=(1.0,-2.0,3.0,-4.0);
        val q2 :Cuaternio=(-2.0,3.0,-1.0,5.0);
        Consola.gotoxy(0,20);
        q.xprintln(Consola);
        q2.xprintln(Consola);
        q.inverso.xprintln(Consola);
        (q/q2).xprintln(Consola);
        (q*q.inverso).xprintln(Consola);
        (q2.inverso*q2).xprintln(Consola);
        (q/q2*q2).xprintln(Consola);
        (q2/q*q).xprintln(Consola);

        val q3 :Cuaternio= -3.0+2.0*i-5.0*j-2.5*k;
        q3.xprintln(Consola);
        Consola.imprime(150,28);*/
      }
}
