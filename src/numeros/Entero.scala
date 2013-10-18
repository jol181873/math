package numeros

;

import estructuras._
import utilidades.Consola;

case class Entero(x :BigInt) extends DominioEuclideo[Entero] with Ordered[Entero]{

  def get :Entero = new Entero(x);

  def esCero     :Boolean = x==0;
  def esUno      :Boolean = x==1;
  def esNegativo :Boolean = x<0;

  def funcionEuclidea = abs;

  def +(that :Entero) :Entero = Entero(x+that.x);
  def -(that :Entero) :Entero = Entero(x-that.x);
  def unary_-         :Entero = Entero(-x);
  def *(that: Entero) :Entero = Entero(x*that.x);
  def /(that :Entero) :Entero = Entero(x/that.x);
  def %(that :Entero) :Entero = Entero(x%that.x);
  def ^(that :Entero) :Entero = {
      var salida=this;
      var contador=uno;
      while(contador!=that) {
          salida=salida*this;
          contador=contador+1;
      }
      salida;
  }
  def abs  :Entero= x.abs;

  def cero :Entero = Entero.Cero;
  def uno  :Entero = Entero.Uno;

  def toBigInt :BigInt = x;

  def esPrimo :Boolean = {
      def factorImpar(n :Entero,contador :Entero) :List[Entero] = {
          if ((n%2).esCero) factorImpar(n/2,contador+1);
          else List(n,contador);
      }
    val nums=factorImpar(this-1,0);
    val a = Entero(2);
    var beta :Entero=a.modPow(nums(0),this);
    if (beta.esUno) return true;
    var contador=cero;
    while(contador!=nums(1)) {
        if (beta==this-1) return true;
        else if (beta.esUno) return false;
        beta=beta.modPow(2,this);
        contador=contador+1;
    }
    false;
  }

  override def toString :String = x.toString;

  override def equals(o :Any) :Boolean = {
      val that = o.asInstanceOf[Entero];
      (this-that).esCero;
  }

  def compare(that :Entero) = x.compare(that);

}

object Entero {
	  implicit object Cero extends Entero(0);
	  object Uno extends Entero(1);

    implicit def int2Entero   (a :Int)    :Entero = Entero(a);
    implicit def bigint2Entero(a :BigInt) :Entero = Entero(a);
    implicit def entero2BigInt(a :Entero) :BigInt = a.toBigInt;

}
