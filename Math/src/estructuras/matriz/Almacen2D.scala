package estructuras.matriz

import collection.mutable.ArraySeq
import excepciones._;
import estructuras.{DominioEuclideo, Cuerpo}
import util.Random

/**
 * Created by IntelliJ IDEA.
 * User: jol
 * Date: 3/01/11
 * Time: 11:13
 * To change this template use File | Settings | File Templates.
 */

abstract class Almacen2D[T <: DominioEuclideo[T]](val m :Int,val n :Int,implicit val cero :T) {

  var filas :Array[Int]
  var colum :Array[Int]

  def get(m :Int,n :Int) :Almacen2D[T];
  def get(m :Int,n :Int,a :Seq[T]) :Almacen2D[T];
  def apply(i :Int,j :Int) :T;
  def update(i :Int, j :Int,valor :T);
 
  def fillTriangularInfEstricta(r :()=>T) = {
    for (i<-1 to m)
      for (j<-1 to n) if (j<i) this(i,j)=r();
    this;
  }
  def fillTriangularSupEstricta(r :()=>T) = {
    for (i<-1 to m)
      for (j<-1 to n) if (j>i) this(i,j)=r();
    this;
  }
  def fillDiagonal(r :()=>T) = {
    for (i<-1 to m)
      for (j<-1 to n) if (j==i) this(i,j)=r();
    this;
  }
  def fillTriangularInf(r :()=>T) = {
    fillTriangularInfEstricta(r).fillDiagonal(r);
  }
  def fillTriangularSup(r :()=>T) = {
    fillTriangularSupEstricta(r).fillDiagonal(r);
  }
  
  def fillSimetrica(r :()=>T) = {
    val a=fillTriangularInf(r);
    for (i<-1 to m) for (j<-1 to n) if (j<i) a(j,i)=a(i,j);
    a;
  }

  def fill(a :T*) = {
    for (i<-1 to m ) for(j<-1 to n) this(i,j)=a((i-1)*n+j-1).get;
    this;
  }

  def fill(rnd :()=>T,max :Int,semilla :Long) = {
    val x=new Random(semilla);
    for (i<-1 to max) this(x.nextInt(m)+1,x.nextInt(n)+1)=rnd();
    this;
  }

  def toList :Seq[T]= {
    var salida = List[List[T]]();
    for (i<-1 to m) {
      var x = List[T]();
      for(j<-1 to n) x=apply(i,j)::x;
      salida=(x.reverse)::salida;
    }
    salida.reverse.flatten;
  }

  def clona :Almacen2D[T]= {
    val salida = get(m,n);
    for(i<-1 to m)
      for(j<-1 to n) salida(i,j)=this(i,j);
    salida;
  }
}

class AlmacenDenso[T <: DominioEuclideo[T]](m2 :Int,n2 :Int,cero2 :T) extends Almacen2D(m2,n2,cero2) {
  private var arr =new ArraySeq[T](m*n);
  for (i<-0 until m*n) arr(i)=cero;
  var filas =new Array[Int](m);
  for (i<-0 until m) filas(i)=0;
  var colum =new Array[Int](n);
  for (i<-0 until n) colum(i)=0;

  def get(m :Int, n :Int) = AlmacenDenso(m,n);
  def get(m :Int, n :Int,a :Seq[T]) = AlmacenDenso[T](m,n,a :_*);

  def apply(i :Int,j :Int) :T = if (i>m || j>n || i<1 || j<1) throw new FueraRangoException(); else arr((i-1)*n+j-1);
  def update(i :Int,j :Int,a :T) = if (i>m || j>n || i<1 || j<1) throw new FueraRangoException();
      else {
        if (!a.esCero && arr((i-1)*n+j-1).esCero) {
          filas(i-1)+=1;
          colum(j-1)+=1;
        } else if (a.esCero && !arr((i-1)*n+j-1).esCero) {
          filas(i-1)-=1;
          colum(j-1)-=1;
        }
        arr((i-1)*n+j-1)=a;
      }

  override def toList = arr.toList;
  override def clona = AlmacenDenso[T](m,n,arr :_*);
}

object AlmacenDenso {
  def apply[T <: DominioEuclideo[T]](m :Int,n :Int)(implicit cero :T) :AlmacenDenso[T] = new AlmacenDenso(m,n,cero);
  def apply[T <: DominioEuclideo[T]](m :Int,n :Int,a :T*)(implicit cero :T) :Almacen2D[T]= AlmacenDenso(m,n).fill(a :_*);
  def apply[T <: DominioEuclideo[T]](m :Int,n :Int,rnd :()=>T,max :Int,semilla :Long)(implicit  cero :T) :Almacen2D[T] = AlmacenDenso(m,n).fill(rnd,max,semilla);
}

class AlmacenHueco[T <: DominioEuclideo[T]](m2 :Int,n2 :Int,cero2 :T) extends Almacen2D(m2,n2,cero2) {
  private var arr :Map[(Int,Int),T]=Map();
  var filas =new Array[Int](m);
  for (i<-0 until m) filas(i)=0;
  var colum =new Array[Int](n);
  for (i<-0 until n) colum(i)=0;

  def get(m :Int,n :Int) = AlmacenHueco(m,n);
  def get(m :Int,n :Int,a :Seq[T]) = AlmacenHueco[T](m,n,a :_*);

  def apply(i :Int,j :Int) :T = if (i>m || j>n || i<1 || j<1) throw new FueraRangoException(); else arr.getOrElse((i,j),cero);
  def update(i :Int, j :Int,a :T)=if (i>m || j>n || i<1 || j<1) throw new FueraRangoException();
      else synchronized{
        if (!a.esCero && !arr.contains((i,j))) {
          filas(i-1)+=1;
          colum(j-1)+=1;
        }
        if (a.esCero) {
          filas(i-1)-=1;
          colum(j-1)-=1;
          arr=arr - ((i,j));
        } else arr+=(i, j)->a;
      }
}

object AlmacenHueco {
  def apply[T <: DominioEuclideo[T]](m :Int,n :Int)(implicit cero :T) :AlmacenHueco[T] = new AlmacenHueco(m,n,cero);
  def apply[T <: DominioEuclideo[T]](m :Int,n :Int,a :T*)(implicit cero :T) :Almacen2D[T]= AlmacenHueco(m,n).fill(a :_*);
  def apply[T <: DominioEuclideo[T]](m :Int,n :Int,rnd :()=>T,max :Int,semilla :Long)(implicit  cero :T) :Almacen2D[T] = AlmacenHueco(m,n).fill(rnd,max,semilla);
}

class AlmacenIdentidad[T <: DominioEuclideo[T]](m2 :Int,n2 :Int,cero2 :T) extends Almacen2D(m2,n2,cero2) {
  var filas =new Array[Int](m);
  for (i<-0 until m) filas(i)=1;
  var colum =new Array[Int](n);
  for (i<-0 until n) colum(i)=1;

  def get(m :Int,n :Int) = AlmacenIdentidad(m,n);
  def get(m :Int,n :Int,a :Seq[T]) = AlmacenIdentidad[T](m,n);

  def apply(i :Int,j :Int) :T = if (i>m || j>n || i<1 || j<1) throw new FueraRangoException(); else 
      if (i==j) cero.uno; else cero;
  def update(i :Int, j :Int,a :T) = throw new AlmacenSoloLectura();

  override def clona :Almacen2D[T]= {
    val salida = AlmacenHueco(m,n);
    for(i<-1 to m)
      for(j<-1 to n) salida(i,j)=this(i,j);
    salida;
  }
}

object AlmacenIdentidad {
  def apply[T <: DominioEuclideo[T]](m :Int,n :Int)(implicit cero :T) :AlmacenIdentidad[T] = new AlmacenIdentidad(m,n,cero);
}