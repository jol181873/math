package estructuras

;

import numeros._
import scala.collection.mutable.ArraySeq
import utilidades._;

class Polinomio[T <: Cuerpo[T]](val grado :Int) extends DominioEuclideo[Polinomio[T]] {

    protected var coeficientes :ArraySeq[T]=new ArraySeq(grado+1);

    def this(l :Seq[T])= {
        this(l.length-1);
        for(i<-0 until l.length){
          coeficientes(i)=l(i);
        }
        //l.copyToArray(coeficientes,0);
    }
    def this(n :Int,cero :T) = {
        this(n);
        for (i<-0 to grado) {
            this(i)=cero;
        }
    }
    def this(n :Int,i :Int,coef :T) = {
        this(n,coef.cero);
        this(i)=coef;
    }
    def this(a :Polinomio[T]) = this(a.toList);

    def esUno  :Boolean = (grado==0 && this(0).esUno);
    def esCero :Boolean = (grado==0 && this(0).esCero);
    def uno  :Polinomio[T] = new Polinomio(0,this(0).uno);
    def cero :Polinomio[T] = new Polinomio(0,this(0).cero);


    def toList :List[T] = coeficientes.toList;

    def apply(i :Int) :T = coeficientes(i);
    def update(i :Int, valor :T) = coeficientes(i)=valor;
    def coef_director :T =coeficientes(grado);
    def get :Polinomio[T] = new Polinomio(coeficientes);

    def funcionEuclidea = grado;

    def reduce :Polinomio[T] = {
        var nocero :Option[T]=toList.reverse.find(!_.esCero);
        nocero match {
            case Some(a) =>new Polinomio(toList.slice(0,toList.lastIndexOf(a)+1));
            case None     =>cero;
        }
    }

    def hazMonico :Polinomio[T] = this.reduce*coef_director.inverso;

    def +(that :Polinomio[T]) :Polinomio[T] ={
        def suma(l1 :List[T],l2 :List[T]) :List[T] = l1 match {
            case Nil  =>l2;
            case o::p =>l2 match {
                            case Nil    =>l1;
                            case o2::p2 => (o+o2)::suma(p,p2);
                       }
         }
         new Polinomio(suma(this.toList,that.toList)).reduce;
    }
    def -(that :Polinomio[T]) :Polinomio[T] = new Polinomio(this+(-that));
    def unary_-               :Polinomio[T] = new Polinomio(toList.map(-_));
    def *(that :T)            :Polinomio[T] = new Polinomio(toList.map(_*that));
    def *(that: Polinomio[T]) :Polinomio[T] = {
        var resultado=new Polinomio(grado+that.grado,this(0).cero);

        for (i<-List.range(0,grado+1)) {
            for (j<-List.range(0,that.grado+1)) {
                resultado(i+j)=resultado(i+j)+this(i)*that(j);
            }
        }
        resultado.reduce;
    }

    def division(that :Polinomio[T]) :(Polinomio[T],Polinomio[T]) = {
        //(cociente,resto)
        if (grado<that.grado) (cero,get)
        else {
          var resultado :Polinomio[T]= new Polinomio(grado-that.grado);
          var monomio :Polinomio[T]=null;
          var dividendo = get

          var a=dividendo.grado
          var b=that.grado

          while (a>=b) {
            var coef=dividendo(a)/that(b);
            monomio=new Polinomio[T](a-b,a-b,coef);
            dividendo=dividendo-that*monomio;
            resultado(a-b)=monomio.coef_director;
            a=a-1;
          }
          (resultado.reduce,dividendo.reduce);
        }
    }

    def /(that :Polinomio[T]) = {
        var temp=division(that);
        temp._1;
    }

    def %(that :Polinomio[T]) = {
        var temp=division(that);
        temp._2;
    }

    override def mcd(that :Polinomio[T]) :Polinomio[T] = super.mcd(that).hazMonico;

    def apply(x :T) :T = {
        def aplica(l :Seq[T]) :T = l match {
          case Nil     => this(0).cero;
          case List(o) => o
          case o::p    => o+x*aplica(p);
        }
        aplica(coeficientes.toList);
    }

    override def toString :String = {
        var resultado :String="";
        var i :Int = 0;
        var signo :String="";
        for(x <-coeficientes) {
            i match {
                case 0 =>resultado=resultado+"+"+x;
                case 1 =>resultado=resultado+"+"+x+"x";
                case i =>resultado=resultado+"+"+x+"x^"+i;
            }
            i=i+1;
        }
        resultado;
    }

    override def xprint(c :Consola) {
      this(0).asInstanceOf[Any] match {
       /* case x :numeros.Real =>
              for(i<-0 to grado)
                  {if (this(i).asInstanceOf[numeros.Real].toDouble>0.0) c.xprint("(+") else c.xprint("(");
                   this(i).xprint(c);
                   c.xprint(") x^"+i);
                  }*/
        case _=>
              for(i<-0 to grado)
                {c.xprint("+");
                 this(i).xprint(c);
                 c.xprint("x^"+i);
                }
      }
    }

}

object Polinomio {

    def apply[T <:Cuerpo[T]](a :T*) = new Polinomio[T](a);

    def x                         :Polinomio[Real] = Polinomio[Real](0.0,1.0);

    implicit def seq2Polinomio[T <:Cuerpo[T]](a :Seq[T]) :Polinomio[T] = new Polinomio[T](a);

    //No funciona
    def ciclotomicoC(n :Int) = {
        var factor :Polinomio[Complejo]=new Polinomio(1,1, Complejo.Uno);
        var salida :Polinomio[Complejo]=null;
        for (i<-List.range(1, n+1)) {
            if (UtilInt.mcd(n,i)==1) {
                factor(0)=Complejo.raizUnidad(n,i)*(-1.0);
                if (i==1) {salida=factor}
                else {salida=(salida*factor)}
                //println(factor);
            }
        }
        salida;
    }

    def ciclotomicoQ(n :Int) = {
         def numerador(n :Int) :Polinomio[Racional[Entero]]={
             var salida :Polinomio[Racional[Entero]]=new Polinomio(n,n, Racional.Uno);
             salida(0)= -1;
             salida;
         }
        def ciclo_interno(n :Int,p :Array[Polinomio[Racional[Entero]]]) :Unit={
            var factor :Polinomio[Racional[Entero]]=Polinomio(Racional.Uno);
            for (i<-1 until n if (n%i==0)) {
                if (p(i-1)==null) {ciclo_interno(i,p);}
                factor=factor*p(i-1);
            }
            var pol :Polinomio[Racional[Entero]]=numerador(n);
            p(n-1)=pol/factor;
        }
        var p :Array[Polinomio[Racional[Entero]]]=new Array(n);
        p(0)=numerador(1);
        ciclo_interno(n,p);
        p(n-1);
    }

}