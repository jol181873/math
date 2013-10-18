package estructuras.matriz

/*
 * CuasiMatriz.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import estructuras._
import scala.actors._;
import scala.util.Random
import utilidades.Consola
import excepciones._;


class CuasiMatriz[T <: DominioEuclideo[T]](val plugin :Almacen2D[T])  {
    val m = plugin.m;
    val n = plugin.n;
    implicit val cero = plugin.cero;
    def esCuadrada = m==n;

    def apply(i :Int,j :Int) :T = plugin(i,j);
    def apply(i :Int,i2 :Int,j :Int,j2 :Int) :CuasiMatriz[T] = {
        var salida = CuasiMatriz[T](plugin.get(i2-i+1,j2-j+1));
        for(a<-1 to salida.m) {
            for(b<-1 to salida.n) salida(a,b)=this(a+i-1,b+j-1);
        }
        salida;
    }
    def update(i :Int, j :Int,a :T) = plugin(i,j) = a;

    def toList = plugin.toList;

    def combinaFila(filaDestino :Int,filaOrigen :Int,factor :T) {
        for(i<-1 to n) this(filaDestino,i)=this(filaOrigen,i)*factor+this(filaDestino,i);
    }

    def multiplicaFila(destino :Int,factor :T) {
        for (i<-1 to n) this(destino,i)=this(destino,i)*factor;
    }
    def subMatriz(i :Int,j :Int) = {
        var salida = CuasiMatriz[T](plugin.get(m-1,n-1));
        var a=1;
        for (i2<-1 to m) {
            if (i!=i2) {
                var b=1;
                for (j2<-1 to n) {
                    if (j!=j2) {
                        salida(a,b)=this(i2,j2);
                        b=b+1;
                    }
                }
                a=a+1;
            }
        }
        salida;
      }

    def +(that :CuasiMatriz[T]) = {
      if (m!=that.m || n!=that.n) throw new DimensionException();
      CuasiMatriz[T](plugin.get(m,n,toList.zip(that.toList).map(x=>x._1+x._2)));
    }

    def -(that :CuasiMatriz[T]) = {
      if (m!=that.m || n!=that.n) throw new DimensionException();
      CuasiMatriz[T](plugin.get(m,n,toList.zip(that.toList).map(x=>x._1-x._2)));
    }

    def *(that :T) = CuasiMatriz[T](plugin.get(m,n,toList.map(_*that)));

    def *(that :CuasiMatriz[T]) = {
        if (n!=that.m) throw new DimensionException();
        var salida = CuasiMatriz[T](plugin.get(m,that.n));
        for (i<-1 to m)
          if (plugin.filas(i-1)!=0)
            for (j<-1 to that.n)
              if (that.plugin.colum(j-1)!=0) {
                salida(i,j)=cero;
                for (k<-1 to n) if (!this(i,k).esCero && !that(k,j).esCero) salida(i,j)=salida(i,j)+this(i,k)*that(k,j);
                //for (k<-1 to n) salida(i,j)=salida(i,j)+this(i,k)*that(k,j);
            }
        salida;
    }

    //Multiplicacion multihilo
    //Una matriz 2500x2500 con 1000 elementos distintos de cero
    //tarda 46 segundos con la multiplicacion normal y
    //35 con la multihilo
    def <*>(that :CuasiMatriz[T]) :CuasiMatriz[T]= {
      if (n!=that.m) throw new DimensionException();
      val salida = CuasiMatriz[T](plugin.get(m,that.n));

      def multFilaCol(i :Int,j :Int) = {
        var suma :T=cero;
        for (col<-1 to n) if (!this(i,col).esCero && !that(col,j).esCero) suma=suma+this(i,col)*that(col,j);
        salida(i,j)=suma
      }

      var lista=List[Future[Unit]]();
      for (i<-(1 to m).toList)
        if (plugin.filas(i-1)!=0)
          for(j<-(1 to that.n).toList)
            if (that.plugin.colum(j-1)!=0) lista=Futures.future{multFilaCol(i,j)}::lista;

      for(f<-lista) f();
      salida;
    }

    //traspuesta
    def unary_! = {
        var salida = CuasiMatriz[T](plugin.get(n,m));
        for(i<-1 to m) {
            for(j<-1 to n) {
                salida(j,i)=this(i,j);
            }
        }
        salida;
    }

    def detMultihilo :T= {
      det;
    }

    def det:T= {
    	if (!esCuadrada) throw new DimensionException();
        if (m==1 && n==1) return this(1,1);
        var suma :T=cero;
        for (j<-1 to n) {
            var temp=this(1,j)*subMatriz(1,j).det;
            if (j%2==0) temp= -temp;
            suma=suma+temp;
        }
        suma;
    }

    override def toString = {
        var salida :String="";
        for (i<-1 to m) {
            for (j<-1 to n) {
                salida=salida+"|"+this(i,j);
            }
            salida=salida+"|\n";
        }
        salida;
    }

    def xprint(c :Consola) {
      var lon=0;
      c.push;
      c.inc(1,0);
      for(i<-1 to m) {
        c.push;
        for(j<-1 to n) {
          c.push;
          this(j,i).xprint(c);
          lon=lon.max(c.x);
          c.pop;
          c.inc(0,6);
        }
        c.pop;
        c.inc(lon+5,0)
      }
    }

  override def equals(b :Any) :Boolean = {
    val c = b.asInstanceOf[CuasiMatriz[T]];
    for (i<-1 to m)
      for (j<-1 to n) if (!(this(i,j)-c(i,j)).esCero) return false;
    return true;
  }
}

object CuasiMatriz {
  def apply[T <: DominioEuclideo[T]](plugin :Almacen2D[T]) :CuasiMatriz[T] = new CuasiMatriz(plugin);
  def apply[T <: DominioEuclideo[T]](a :CuasiMatriz[T]) :CuasiMatriz[T] = CuasiMatriz(a.plugin.clona)
  def apply[T <: DominioEuclideo[T]](m :Int,n :Int, a :T*)(implicit cero :T) :CuasiMatriz[T] = new CuasiMatriz(AlmacenDenso[T](m,n,a :_*));

  implicit def cuasi2Mat[T <: Cuerpo[T]](a :CuasiMatriz[T]) :Matriz[T] = Matriz(a);
}
