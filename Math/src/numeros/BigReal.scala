package numeros;

import estructuras._
import normado._
import vector._
import excepciones._
import scala.math._
import java.math.MathContext
import java.math.RoundingMode

case class BigReal(x :BigDecimal) extends Cuerpo[BigReal] with EspacioNormado with EspacioVectorial[BigReal,BigReal] with Ordered[BigReal] {

  def esCero    :Boolean = x.abs<Real.precision;
  def esUno     :Boolean = x==1.0;
  def esNegativo:Boolean = x<0.0;

  def +(that: BigReal)  :BigReal = x+that.x;
  def -(that: BigReal)  :BigReal = x-that.x;
  def unary_-           :BigReal = -x;
  def *(that: BigReal)  :BigReal = x*that.x;
  def /(that: BigReal)  :BigReal = x/that.x;
  def inverso           :BigReal = if (esCero) throw new NoTieneInversoException(); else BigDecimal(1.0)/x;

  def dim :Int = 1;
  def base :List[BigReal] = List(uno);

  def cero                    :BigReal = BigReal.Cero;
  def uno                     :BigReal = BigReal.Uno;
  def get                     :BigReal = x;
  def get(precision :Int)	    :BigReal = BigDecimal(x.toString,new MathContext(precision,RoundingMode.HALF_UP))
  def conjugado               :BigReal = get;
  def Re                      :Real    = get;
  def norma2                  :Real    = x.abs;
  def cos                     :BigReal = scala.math.cos(x.toDouble);
  def arccos                  :BigReal = scala.math.acos(x.toDouble);
  def sen                     :BigReal = scala.math.sin(x.toDouble);
  def arcsen                  :BigReal = scala.math.asin(x.toDouble);
  def tan                     :BigReal = scala.math.tan(x.toDouble);
  def arctan                  :BigReal = scala.math.atan(x.toDouble);

  def sqr                     :BigReal = x*x;
  def sqrt                    :BigReal = scala.math.sqrt(x.toDouble);

  def exp                     :BigReal = scala.math.exp(x.toDouble);
  def log                     :BigReal = scala.math.log(x.toDouble);

  def toDouble :Double = x.toDouble;

  def toBigDec :BigDecimal = x;

  override def toString :String = x.toString;

  override def equals(o :Any) :Boolean = {
      val that = o.asInstanceOf[BigReal];
      (this-that).esCero
   }

  def compare(that :BigReal) :Int = x.compare(that.x);
}

object BigReal {
	implicit object Cero extends BigReal(0.0);
	object Uno extends BigReal(1.0);

	val precision 	:BigReal = 1e-9;

    implicit def int2Real     (a :Int)              :BigReal = BigReal(a);
    implicit def bigint2Real  (a :BigInt)           :BigReal = BigReal(BigDecimal(a))
    implicit def bigdec2Real  (a :BigDecimal)       :BigReal = BigReal(a)
    implicit def double2Real  (a :Double)           :BigReal = BigReal(a);
    implicit def entero2Real  (a :Entero)           :BigReal = BigReal(BigDecimal(a.toBigInt))
    implicit def racional2Real(a :Racional[Entero]) :BigReal = BigReal(BigDecimal(a.getNum.toBigInt)/BigDecimal(a.getDen.toBigInt));
    implicit def real2Double  (a :BigReal) 			    :Double = a.toDouble;
    implicit def real2BigDec  (a :BigReal) 			    :BigDecimal = a.toBigDec;
    implicit def big2Real     (a :BigReal)          :Real = a.toDouble;
}
