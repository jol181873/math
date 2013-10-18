package estructuras.vector

import util.Random
import estructuras.Cuerpo
import collection.mutable.ArraySeq
import excepciones.FueraRangoException

abstract class Almacen1D[T <: Cuerpo[T]](val dim :Int,implicit val cero :T) {

  def get(m :Int) :Almacen1D[T];
  def get(a :Seq[T]) :Almacen1D[T];
  def apply(i :Int) :T;
  def update(i :Int,valor :T);
  def toList :Seq[T]
  def clona :Almacen1D[T];

  def fill(a :T*) = {
    for (i<-1 to dim) this(i)=a(i-1).get;
    this;
  }

  def fill(rnd :()=>T,max :Int,semilla :Long) = {
    val x=new Random(semilla);
    for (i<-1 to max) this(x.nextInt(dim)+1)=rnd();
    this;
  }
}

class AlmacenDensoVec[T <: Cuerpo[T]](dim2 :Int,cero2 :T) extends Almacen1D(dim2,cero2) {
  private var arr =new ArraySeq[T](dim);
  for (i<-0 until dim) arr(i)=cero;

  def get(m :Int) = AlmacenDensoVec(m);
  def get(a :Seq[T]) = AlmacenDensoVec(a :_*);

  def apply(i :Int) :T = if (i>dim || i<1) throw new FueraRangoException(); else arr(i-1);
  def update(i :Int,a :T) = if (i>dim || i<1) throw new FueraRangoException(); else arr(i-1)=a;

  def toList = arr.toList;
  def clona = AlmacenDensoVec(arr :_*);
}

object AlmacenDensoVec {
  def apply[T <: Cuerpo[T]](m :Int)(implicit cero :T) :AlmacenDensoVec[T] = new AlmacenDensoVec(m,cero);
  def apply[T <: Cuerpo[T]](a :T*)(implicit cero :T) :Almacen1D[T]= AlmacenDensoVec(a.length).fill(a :_*);
  def apply[T <: Cuerpo[T]](m :Int,rnd :()=>T,max :Int,semilla :Long)(implicit  cero :T) :Almacen1D[T] = AlmacenDensoVec(m).fill(rnd,max,semilla);
}

class AlmacenHuecoVec[T <: Cuerpo[T]](dim2 :Int,cero2 :T) extends Almacen1D(dim2,cero2) {
  private var arr :Map[Int,T]=Map();

  def get(m :Int) = AlmacenHuecoVec(m);
  def get(a :Seq[T]) = AlmacenHuecoVec(a :_*);

  def apply(i :Int) :T = if (i>dim || i<1) throw new FueraRangoException(); else arr.getOrElse(i,cero);
  def update(i :Int,a :T)=if (i>dim ||i<1) throw new FueraRangoException();
    else synchronized{
      if (a.esCero) arr-=i; else arr+=i->a;
    }

  def toList = {
    var salida = List[T]();
    for(i<-1 to dim) salida=this(i)::salida;
    salida.reverse;
  }
  def clona = {
    val salida = AlmacenHuecoVec(dim);
    for(i<-1 to dim) salida(i)=this(i);
    salida;
  }
}

object AlmacenHuecoVec {
  def apply[T <: Cuerpo[T]](m :Int)(implicit cero :T) :AlmacenHuecoVec[T] = new AlmacenHuecoVec(m,cero);
  def apply[T <: Cuerpo[T]](a :T*)(implicit cero :T) :Almacen1D[T]= AlmacenHuecoVec(a.length).fill(a :_*);
  def apply[T <: Cuerpo[T]](m :Int,rnd :()=>T,max :Int,semilla :Long)(implicit  cero :T) :Almacen1D[T] = AlmacenHuecoVec(m).fill(rnd,max,semilla);
}
