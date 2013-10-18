package aproximacion

/*
 * Bezier.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

;

import estructuras.vector._
import numeros.Real;

object Bezier {

    def bezier(p :Seq[Kn[Real]],np :Int) :Seq[Kn[Real]] = {
        val h=1.0/np;
        val n=p.length;
        var salida :List[Kn[Real]]=Nil;
        for(l<-0 to np) {
            var t=l*h;
            var q =new Array[Kn[Real]](n);
            p.copyToArray(q,0);
            for(j<-1 to n) for(i<-0 until n-j) q(i)=(q(i)*(1-t)+q(i+1)*t);
            salida=q(0)::salida;
        }
        salida.reverse;
    }
}
