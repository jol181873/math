package numeros;

import estructuras.Cuerpo;
import estructuras.matriz.{AlmacenDenso, Matriz}
import estructuras.normado.EspacioNormado
import estructuras.vector.{EspacioVectorial, R3}
//import org.scalatest.FlatSpec
//import org.scalatest.matchers.ShouldMatchers
import scala.util.Random
import excepciones._;
//import org.scalatest.prop.Checkers
//import org.scalacheck._;
//import org.scalacheck.Prop.forAll;

/**
 * Created by IntelliJ IDEA.
 * User: jol
 * Date: 07-dic-2009
 * Time: 10:55:37
 * To change this template use File | Settings | File Templates.
 */

case class Cuaternio(d :Real,a :Real,b :Real,c :Real) extends Cuerpo[Cuaternio] with EspacioNormado with EspacioVectorial[Real,Cuaternio]{

    val u = R3(a,b,c);

    def this(d :Real,u :R3)       {this(d,u.getX,u.getY,u.getZ);}
    def this(a :Matriz[Real])     {this(a(1,1),a(2,1),a(4,1),a(1,3));}
    //def this(a :Matriz[Complejo]) {this(a(1,1).re,a(2,1).re,a(2,1).im,a(2,2).im);}

    def esCero :Boolean = sqrnorma < Real.precision
    def esUno  :Boolean = (d.esUno && a.esCero && b.esCero && c.esCero);

    def +(that: Cuaternio) :Cuaternio = Cuaternio(d+that.d,u+that.u);
    def -(that: Cuaternio) :Cuaternio = Cuaternio(d-that.d,u-that.u);
    def unary_-            :Cuaternio = Cuaternio(-d,-a,-b,-c);
    def *(that: Cuaternio) :Cuaternio = Cuaternio(d*that.d-u*that.u,that.u*d+u*that.d+u**that.u);
    def *(that: Real)      :Cuaternio = Cuaternio(d*that,u*that);
    def /(that: Real)      :Cuaternio = Cuaternio(d/that,u*that.inverso);
    def /(that: Cuaternio) :Cuaternio = this*that.inverso;
    def inverso            :Cuaternio = if (esCero) throw new NoTieneInversoException(); else conjugado/sqrnorma;

    def dim :Int = 4;
    def base :List[Cuaternio] = List(uno,i,j,k);

    def cero               :Cuaternio = Cuaternio.Cero;
    def uno                :Cuaternio = Cuaternio.Uno;
    def i                  :Cuaternio = Cuaternio.i;
    def j                  :Cuaternio = Cuaternio.j;
    def k                  :Cuaternio = Cuaternio.k;
    def get                :Cuaternio = Cuaternio(d,u);
    def conjugado          :Cuaternio = Cuaternio(d,-u);

    def Re                 :Real      = d;
    def sqrnorma           :Real 	  = (this*conjugado).Re;
    def norma2             :Real 	  = sqrnorma.sqrt;

    def toMatrizCompleja   :Matriz[Complejo] = Matriz[Complejo](AlmacenDenso[Complejo](2,2,Complejo(d,-c),Complejo(-a,b), Complejo(a,b),Complejo(d,c))(Complejo.Cero));
    def toMatrizReal       :Matriz[Real]     = Matriz[Real](AlmacenDenso[Real](4,4,d,-a,c,-b, a,d,-b,-c, -c,b,d,-a, b,c,a,d)(Real.Cero));

    def *(punto :R3)                    :R3 = (this*Cuaternio(0.0,punto)*this.conjugado).u;
    def giraPunto(origen :R3,punto :R3) :R3 = *(punto-origen)+origen;

    override def toString = d.toString+"+"+a.toString+"i+"+b.toString+"j+"+c.toString+"k";

    override def equals(o :Any) :Boolean = {
      val that = o.asInstanceOf[Cuaternio];
      (this-that).esCero
    }
}

object Cuaternio {
    def apply(d :Real,u :R3) = new Cuaternio(d,u);

    implicit object Cero extends Cuaternio(0.0,0.0,0.0,0.0);
    object Uno extends Cuaternio(1.0,0.0,0.0,0.0);
    object i extends Cuaternio(0.0,1.0,0.0,0.0);
    object j extends Cuaternio(0.0,0.0,1.0,0.0);
    object k extends Cuaternio(0.0,0.0,0.0,1.0);

    implicit def int2Cuaternio     (a :Int)                           :Cuaternio = Cuaternio(a,0.0,0.0,0.0);
    implicit def entero2Cuaternio  (a :Entero)                        :Cuaternio = Cuaternio(a,0.0,0.0,0.0);
    implicit def double2Cuaternio  (a :Double)                        :Cuaternio = Cuaternio(a,0.0,0.0,0.0);
    implicit def tupla2Cuaternio   (a :(Double,Double,Double,Double)) :Cuaternio = Cuaternio(a._1,a._2,a._3,a._4);
    implicit def racional2Cuaternio(a :Racional[Entero])              :Cuaternio = Cuaternio(a,0.0,0.0,0.0);
    implicit def real2Cuaternio    (a :Real)                          :Cuaternio = Cuaternio(a,0.0,0.0,0.0);

    def random :Cuaternio = {
      val r = new Random(System.currentTimeMillis);
      Cuaternio(r.nextDouble,r.nextDouble,r.nextDouble,r.nextDouble);
    }

    def creaCuaternioGiro(angulo :Real,eje :R3) :Cuaternio = {
      val tetha=angulo/2;
      Cuaternio(tetha.cos,eje*(eje.norma2.inverso)*tetha.sen);
    }
    def giraPunto(angulo :Real,eje :R3,punto :R3) :R3 = {
      val q=Cuaternio.creaCuaternioGiro(angulo,eje);
      (q*Cuaternio(0.0,punto)*q.conjugado).u;
    }
    def giraPunto(angulo :Real,eje :R3,origen :R3,punto :R3) :R3 = {
      val q=Cuaternio.creaCuaternioGiro(angulo,eje);
      (q*Cuaternio(0.0,punto-origen)*q.conjugado).u+origen;
    }
}

/*object CuaternioTests extends Properties("Cuaternio") {
  val doubles =Arbitrary.arbitrary[Double];

  val doubleNoNulo = doubles suchThat ( !Real(_).esCero);

  val cuaternioNoNulo = for {
    d <- doubleNoNulo
    a <- doubles
    b <- doubles
    c <- doubles
  } yield Cuaternio(d,a,b,c);

  val cuaternios = for {
    d <- doubles
    a <- doubles
    b <- doubles
    c <- doubles
  } yield Cuaternio(d,a,b,c);

  property("Un cuaternio creado Cuaternio(d,a,b,c) debe ser igual a Cuaternio(d,R3(a,b,c))") =
	  forAll((d :Double,a :Double,b :Double,c :Double) => Cuaternio(d,a,b,c)==Cuaternio(d,R3(a,b,c)));

  property("La suma de cuaternios debe ser conmutativa") =
  	  forAll(cuaternios,cuaternios)( (a,b) => (a+b)==(b+a))

  property("La suma de cuaternios debe ser asociativa") =
  	  forAll(cuaternios,cuaternios,cuaternios)( (a,b,c) =>{
  	    val x= (a+b)+c
  	    val y= a+(b+c)

  	    x==y
  	  })
  property("La suma de cuaternios tener al cero como elemento neutro") =
	  forAll(cuaternios)(a => (a+Cuaternio.Cero)==a);

  property( "Todo Cuaternio tener opuesto valido") =
	  forAll(cuaternios)(a => (a + -a).esCero);


  property("Todo Cuaternio no nulo tiene inverso valido") =
      forAll(cuaternioNoNulo)(a=> a*a.inverso==Cuaternio.Uno);

  property("El cuaternio definido mediante matriz real y compleja debe coincidir con el creado normalmente") =
	  forAll((d :Double,a :Double,b :Double,c :Double) => {
          val C=Cuaternio(d,a,b,c)
          val D= new Cuaternio(C.toMatrizReal);
          //val E= new Cuaternio(C.toMatrizCompleja);
          C==D
        }
        )

  property("Las operaciones producto y suma mediante matrices real y compleja debe coincidir con las operaciones normales") =
	  forAll(cuaternios,cuaternios)( (a,b)=> {
          val f = a.toMatrizReal;
          val g = b.toMatrizReal;
          //val b = C.toMatrizCompleja;
          //val b2= D.toMatrizCompleja;

          (a+b)==new Cuaternio(f+g) && (a*b)==new Cuaternio(f*g);

          //(C+D)==(new Cuaternio(b+b2));
          //(C*D)==(new Cuaternio(b*b2));
        }
        )
}
*/