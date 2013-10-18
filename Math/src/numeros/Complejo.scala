package numeros

/*
 * Complejo.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

;

import estructuras._;
import normado._;
import vector._;
import excepciones._;

case class Complejo(re :Real,im :Real) extends Cuerpo[Complejo] with EspacioNormado with EspacioVectorial[Real,Complejo]{

    def esCero :Boolean = (re.esCero && im.esCero);
    def esUno  :Boolean = (re.esUno && im.esCero);

    def +(that: Complejo) :Complejo = Complejo(re+that.re,im+that.im);
    def -(that: Complejo) :Complejo = Complejo(re-that.re,im-that.im);
    def unary_-           :Complejo = Complejo(-re,-im);
    def *(that: Complejo) :Complejo = Complejo(re*that.re-im*that.im,re*that.im+im*that.re);
    def *(that: Real)     :Complejo = Complejo(re*that,im*that);
    def /(that: Real)     :Complejo = Complejo(re/that,im/that);
    def /(that: Complejo) :Complejo = this*that.inverso;
    def inverso           :Complejo = if (esCero) throw new NoTieneInversoException(); else conjugado/sqrnorma

    def dim :Int = 2;
    def base :List[Complejo] = List(uno,i);

    def cero                :Complejo = Complejo.Cero;
    def uno                 :Complejo = Complejo.Uno;
    def i                   :Complejo = Complejo.i;
    def get                 :Complejo = Complejo(re,im);
    def conjugado           :Complejo = Complejo(re,-im);
    def raiz(n :Int,i :Int) :Complejo = Complejo.makeModuloArgumental(Math.pow(norma2,1/n),(argumento+2*Math.PI*i)/n);
    def sqrt                :Complejo = raiz(2,0);
    def exp                 :Complejo = Complejo(Math.cos(im),Math.sin(im))*Math.exp(re);
    def log                 :Complejo = Complejo(Math.log(norma2),argumento);

    def sqrnorma    :Real = re*re+im*im;
    def norma2      :Real = sqrnorma.sqrt;
    def argumento   :Real = Math.atan2(im, re);

    def Re          :Real = re;
    def Im          :Real = im;

    override def toString :String = {
        var salida:String=null;
        if (im==Real.Cero) {salida=re.toString;}
        else if (re==Real.Cero) {salida=im.toString+"i";}
        else {salida="("+re.toString+"+"+im.toString+"i"+")";}
        salida;
    }

    override def equals(o :Any) :Boolean = {
      val that = o.asInstanceOf[Complejo];
      (this-that).esCero;
    }
}

object Complejo {
    implicit object Cero extends Complejo(0.0,0.0);
    object Uno extends Complejo(1.0,0.0);
    object i extends Complejo(0.0,1.0);

    def makeModuloArgumental(mod :Double,arg :Double) :Complejo = new Complejo(mod*Math.cos(arg),mod*Math.sin(arg));
    def raizUnidad(n :Int,i :Int)                     :Complejo = Uno.raiz(n, i);

    implicit def int2Complejo       (a :Int)                :Complejo = Complejo(a.toDouble,0.0);
    implicit def entero2Complejo    (a :Entero)             :Complejo = Complejo(a.toBigInt.doubleValue,0.0);
    implicit def double2Complejo    (a :Double)             :Complejo = Complejo(a,0.0);
    implicit def tupla2Complejo     (a :(Double,Double))    :Complejo = Complejo(a._1,a._2);
    implicit def racional2Complejo  (a :Racional[Entero])   :Complejo = Complejo(a.getNum.toBigInt.doubleValue/a.getDen.toBigInt.doubleValue,0.0);
    implicit def real2Complejo      (a :Real)               :Complejo = Complejo(a,0.0);

}

