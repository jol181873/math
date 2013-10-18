package numeros

;

import estructuras._
import excepciones._;

case class EnteroModular(var x :Entero,modulo :Entero) extends Cuerpo[EnteroModular] with GrupoFinitoAbeliano[EnteroModular] with GrupoFinitoNoAbeliano[EnteroModular]{
  x=x%modulo;
  if (x.esNegativo) x=modulo+x;

  def get :EnteroModular = new EnteroModular(x,modulo);

  def esCero     :Boolean = x==Entero.Cero;
  def esUno      :Boolean = x==Entero.Uno;

  override def funcionEuclidea = x.abs;

  def +(that :EnteroModular) :EnteroModular = EnteroModular(x+that.x,modulo);
  def -(that :EnteroModular) :EnteroModular = EnteroModular(x-that.x,modulo);
  def unary_-                :EnteroModular = EnteroModular(-x,modulo);
  def *(that: EnteroModular) :EnteroModular = EnteroModular(x*that.x,modulo);
  def /(that :EnteroModular) :EnteroModular = EnteroModular(x/that.x,modulo);
  def inverso                :EnteroModular = {
    val tup = x.coeficientesBezout(modulo);
    if (!tup._3.esUno) throw new NoTieneInversoException();
    EnteroModular(tup._1,modulo);
  }
  def esUnidad :Boolean = {
    val tup = x.coeficientesBezout(modulo);
    if (!tup._3.esUno) return false;
    return true;
  }
  override def %(that :EnteroModular) :EnteroModular = EnteroModular(x%that.x,modulo);
  def ^(that :EnteroModular) :EnteroModular = {
      var salida=this;
      var contador=Entero.Uno.asInstanceOf[Entero];
      while(contador!=that) {
          salida=salida*this;
          contador=contador+1;
      }
      salida;
  }

  def cero :EnteroModular = EnteroModular.cero(modulo);
  def uno  :EnteroModular = EnteroModular.uno(modulo);
      
  def toEntero :Entero = x;

  def conjugado :EnteroModular = get;
  def Re        :Real = x;

  override def toString :String = "["+x.toString+"]";

  override def equals(o :Any) :Boolean = {
      val that = o.asInstanceOf[EnteroModular];
      (this-that).esCero;
  }
}

object EnteroModular {
  
    var modulo :Entero=_;
    
    implicit def cero :EnteroModular = new EnteroModular(0,modulo);
    
    implicit def intAModularConModulo(x :Int) = new EnteroModular(x,modulo);
    
    implicit def enteroAModularConModulo(x :Entero) = new EnteroModular(x,modulo);
    
    implicit def intAModular(x :Int) = new {
      def mod(modulo :Int) = new EnteroModular(x,modulo);
    }
    
    implicit def enteroAModular(x :Entero) = new {
      def mod(modulo :Entero) = new EnteroModular(x,modulo);
    }

    def cero(modulo :Entero)  :EnteroModular = EnteroModular(0,modulo);
    def uno (m :Entero=modulo)  :EnteroModular = EnteroModular(1,m);
  
    def esCuerpo(m :Entero=modulo) :Boolean = m.esPrimo;

    def unidades(m :Entero=modulo) :List[EnteroModular] = {
	    var resultado = List[EnteroModular]();
	    for(elemento<-Entero.Uno until m) 
	      if (elemento.gcd(m)==Entero.Uno.toBigInt) 
	        resultado=EnteroModular(elemento,m)::resultado;
	    
	    resultado.reverse;
	  }
    
    def noUnidades(m :Entero=modulo) :List[EnteroModular] = {
	    var resultado = List[EnteroModular]();
	    for(elemento<-Entero.Uno until m) 
	      if (elemento.gcd(m)!=Entero.Uno.toBigInt) 
	        resultado=EnteroModular(elemento,m)::resultado;
	    
	    resultado.reverse;
	  }
}

