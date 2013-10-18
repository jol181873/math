package estructuras

/*
 * DominioEuclideo.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import matriz.{AlmacenDenso, CuasiMatriz}
import numeros.Entero

trait DominioEuclideo[T <: DominioEuclideo[T]] extends DFU[T] {

    def /(that :T) :T;
    def %(that :T) :T;
    def funcionEuclidea :Entero;

    override def mcd(that :T) :T={
        def mcd_interno(a :T,b :T) :T= {
            val resto :T=a%b;
            if (resto.esCero) {b;}
            else {mcd_interno(b,resto);}
        }
        mcd_interno(this.get,that);
    }

    def coeficientesBezout(that :T) :(T,T,T) = {
      var a :CuasiMatriz[T]= CuasiMatriz(AlmacenDenso(2,3,uno,cero,this.get, cero,uno,that)(cero));
      var i=1;
      var j=2;
      while (!a(j,3).esCero) {
        val factor = a(i,3)/a(j,3);
        a.combinaFila(i,j,-factor);
        val k=i;
        i=j;
        j=k;
      }
      (a(i,1),a(i,2),a(i,3));

    }

    override def mcm(that :T) :T ={
        this*that/mcd(that);
    }

}
