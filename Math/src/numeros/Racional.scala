package numeros

;

import estructuras._;
import utilidades.Consola;
import excepciones._;

case class Racional[T <:DominioEuclideo[T]](var numer :T,var denom :T) extends Cuerpo[Racional[T]] {
  if (denom.esCero) throw new DivisionPorCeroException();
  private val mcd = numer.mcd(denom);
  numer = numer /mcd;
  denom = denom /mcd;

  def getNum :T = numer;
  def getDen :T = denom;

  def esCero :Boolean = numer.esCero;
  def esUno  :Boolean = (numer.esUno && denom.esUno);

  def +(that: Racional[T]) :Racional[T] = Racional(numer * that.denom + that.numer * denom,denom * that.denom);
  def -(that: Racional[T]) :Racional[T] = Racional(numer * that.denom - that.numer * denom,denom * that.denom);
  def unary_-              :Racional[T] = Racional(-numer,denom);
  def *(that: Racional[T]) :Racional[T] = Racional(numer * that.numer, denom * that.denom);
  def /(that: Racional[T]) :Racional[T] = Racional(numer * that.denom, denom * that.numer);
  def inverso              :Racional[T] = if (esCero) throw new NoTieneInversoException(); else Racional(denom,numer);

  def cero      :Racional[T] = Racional(numer.cero,denom.uno);
  def uno       :Racional[T] = Racional(numer.uno,denom.uno);
  def get       :Racional[T] = new Racional(numer,denom);
  def conjugado :Racional[T] = get;
  def Re        :Real        = (numer,denom) match {
    case (numerador :Entero,denominador :Entero) => numerador.toBigInt.doubleValue/denominador.toBigInt.doubleValue;
  }


  def toModulo(modulo :Entero) :EnteroModular = (numer,denom) match {
    case (numerador :Entero,denominador :Entero) => val m=EnteroModular(denominador,modulo).inverso;
                                                    val n=EnteroModular(numerador,modulo);
                                                    m*n;
  }
  override def toString :String = {
      var temp=numer%denom;
      var salida :String=null;
      if (temp.esCero) {salida=(numer/denom).toString;}
      else {salida=numer.toString+"/"+denom.toString;}
      salida;
  }

  override def xprint(c :Consola) {
    numer.asInstanceOf[Any] match {
      case x :Entero =>
          val n = getNum.toString;
          val m = getDen.toString;
          val suma=n.length-m.length;
          val cadena1=(1 to suma).foldLeft("")((a,b)=>a+" ");
          val cadena2=(1 to -suma).foldLeft("")((a,b)=>a+" ");
          c.push;
          c.xprint("|"+n+cadena2+"|");
          c.pop;
          c.inc(0,1);
          c.xprint("|"+m+cadena1+"|");
          c.inc(0,-1);
      case x :Polinomio[Racional[Entero]] =>
          val x0=c.x;
          c.push;
          getNum.xprint(c);
          var lon=c.x-x0;
          c.top;
          c.inc(0,3);
          getDen.xprint(c);
          lon=(c.x-x0).max(lon);
          c.pop;
          c.inc(0,2);
          (1 to lon).foreach(a=>{c.xprint("-"); c.x=c.x-1;});
    }
  }

  override def equals(o :Any) :Boolean = {
      val that = o.asInstanceOf[Racional[T]];
      (this-that).esCero;
  }
}

object Racional {
	  implicit object Cero extends Racional[Entero](0,1);
	  object Uno extends Racional[Entero](1,0);

    implicit def int2Racional   (a :Int)    :Racional[Entero] = Racional(a,1);
    implicit def entero2Racional(a :Entero) :Racional[Entero] = Racional(a,1);
}

