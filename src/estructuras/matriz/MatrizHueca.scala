package estructuras.matriz

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import excepciones._
import estructuras._;
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Random


/*class MatrizHueca[T <: Cuerpo[T]](private val m :Int,private val n :Int,cero :T) extends Matriz[T](m,n,cero){
  private val datos = new HashMap[(Int,Int),T]();
  private var filas =new ListBuffer[Int]();
  private var colum =new ListBuffer[Int]();

  def this(a :CuasiMatriz[T],cero :T) {
    this(a.getM,a.getN,cero);
    for(i<-1 to m)
      for(j<-1 to n) this(i,j)=a(i,j);
  }

  def this(m :Int,n :Int,cero :T,rnd :(Unit=>T),max :Int,semilla :Long){
    this(m,n,cero);
    val x=new Random(semilla);
    for (i<-1 to max) this(x.nextInt(m)+1,x.nextInt(n)+1)=rnd();
  }

  override def apply(i :Int,j :Int) :T = if (i>m || j>n || i<1 || j<1) throw new FueraRangoException();
  else if (datos.isDefinedAt((i,j))) datos((i,j)); else cero;

  override def update(i :Int, j :Int,a :T) =if (i>m || j>n || i<1 || j<1) throw new FueraRangoException(); else
    if (!a.esCero) {
      datos+=(i,j)->a;
      if (!filas.exists(_==i)) filas+=i;
      if (!colum.exists(_==j)) colum+=j;
  }

  def <*>(that :MatrizHueca[T]) = {
        if (n!=that.getM) throw new DimensionException();
        var salida = new MatrizHueca[T](m,that.getN,cero);
        for (i<-1 to m)
          if (filas.exists(_==i))
            for (j<-1 to that.getN)
                if (that.colum.exists(_==j)) {
                  salida(i,j)=cero;
                  for (k<-1 to n) salida(i,j)=salida(i,j)+this(i,k)*that(k,j);
                }
        salida;
    }
 override def toListFlatten = {
   var lista :List[T]=Nil;
   for (i<-1 to m)
     for (j<-1 to n) lista=this(i,j)::lista;
   lista.reverse
 }
}*/
