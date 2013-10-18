package numeros

;

import estructuras._
import normado._
import vector._
import excepciones._
import scala.math._
import java.math.MathContext
import java.math.RoundingMode
import util.Random

case class Real(x :Double) extends Cuerpo[Real] with EspacioNormado with EspacioVectorial[Real,Real] with Ordered[Real] {

  def esCero    :Boolean = x.abs<Real.precision;
  //def esCero    :Boolean = x.abs==0.0;
  def esUno     :Boolean = x==1.0;
  def esNegativo:Boolean = x<0.0;

  def +(that: Real)  :Real = x+that.x;
  def -(that: Real)  :Real = x-that.x;
  def unary_-        :Real = -x;
  def *(that: Real)  :Real = x*that.x;
  def /(that: Real)  :Real = x/that.x;
  def inverso        :Real = if (esCero) throw new NoTieneInversoException(); else 1.0/x;

  def dim :Int = 1;
  def base :List[Real] = List(uno);

  def cero                    :Real = Real.Cero;
  def uno                     :Real = Real.Uno;
  def get                     :Real = x;
  def conjugado               :Real = get;
  def Re                      :Real = get;
  def norma2                  :Real = x.abs;
  def cos                     :Real = scala.math.cos(x);
  def arccos                  :Real = scala.math.acos(x);
  def sen                     :Real = scala.math.sin(x);
  def arcsen                  :Real = scala.math.asin(x);
  def tan                     :Real = scala.math.tan(x);
  def arctan                  :Real = scala.math.atan(x);

  def sqr                     :Real = x*x;
  def sqrt                    :Real = scala.math.sqrt(x);

  def exp                     :Real = scala.math.exp(x);
  def log                     :Real = scala.math.log(x);

  def toDouble :Double = x;

  override def toString :String = x.toString;

  override def equals(o :Any) :Boolean = {
      val that = o.asInstanceOf[Real];
      (this-that).esCero
   }

  def compare(that :Real) :Int = x.compare(that.x);
}

object Real {
  implicit object Cero extends Real(0.0);
  object Uno extends Real(1.0);

  val precision 	:Real = 1e-9;

  implicit def int2Real     (a :Int)              :Real = Real(a);
  implicit def bigint2Real  (a :BigInt)           :Real = Real(a.toDouble)
  implicit def bigdec2Real  (a :BigDecimal)       :Real = Real(a.toDouble)
  implicit def double2Real  (a :Double)           :Real = Real(a);
  implicit def entero2Real  (a :Entero)           :Real = a.toBigInt
  implicit def racional2Real(a :Racional[Entero]) :Real = BigDecimal(a.getNum.toBigInt)/BigDecimal(a.getDen.toBigInt);
  implicit def real2Double  (a :Real) 			      :Double = a.toDouble;
  implicit def real2BigDec  (a :Real)   			    :BigDecimal = a.toDouble;

  val rand=new Random(System.currentTimeMillis());
  def randomRealPositivo(min :Real,max :Real) :Real = {
    var a :Real=Real.Cero;
    while (a.esCero) {
    	a=min+rand.nextDouble()*(max-min);
    }
    if (a>0) a 
    else if (a<0) -a
    else 1.0;
  }

}
