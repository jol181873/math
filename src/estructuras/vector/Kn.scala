package estructuras.vector

/*
 * Kn.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import java.io._;
import estructuras._;
import matriz._;
import normado._;
import numeros._;
import scala.collection.mutable.ArraySeq


class Kn[T <: Cuerpo[T]](val plugin :Almacen1D[T]) extends EspacioVectorial[T,Kn[T]] with EspacioNormado {
  val dim = plugin.dim;
  implicit val cero=plugin.cero;

  def toList :Seq[T] = plugin.toList;

  def apply(i :Int) :T = plugin(i);
  def update(i :Int, valor :T) = plugin(i)=valor;

  def get :Kn[T] = Kn(plugin.clona);

  def base :List[Kn[T]] = {
      var salida :List[Kn[T]]= Nil;
      for (i<-1 to dim){
          var vec = Kn(plugin.get(dim));
          for (j<-1 to dim) vec(j)=cero;
          vec(i)=cero.uno;
          salida=vec::salida;
      }
      salida.reverse;
  }

  private def suma(l :Seq[T],m :Seq[T]) :Seq[T] = l.zip(m).map(i=>i._1+i._2);

  def unary_-                 :Kn[T] = Kn(plugin.get(toList.map(-_)));
  def +(that :Kn[T])          :Kn[T] = Kn(plugin.get(suma(toList,that.toList)));
  def -(that :Kn[T])          :Kn[T] = Kn(plugin.get(suma(toList,(-that).toList)));
  //producto escalar
  def *(that: Kn[T])          :T     = toList.zip(that.toList).map(a=>a._1.conjugado*a._2).foldLeft(cero)((a,b)=>a+b)
  //producto externo
  def *(that :T)              :Kn[T] = Kn(plugin.get(toList.map(a=>that*a)));

  override def norma1         :Real  = toList.foldLeft(Real.Cero.asInstanceOf[Real])((a,b)=>a+(b*b.conjugado).Re.sqrt)
  def norma2                  :Real  = (this*this).Re.sqrt;
  override def normaInfinito  :Real  = {
    var max :Real= Real.Cero;
    for(i<-toList) {
      val norma = (i*i.conjugado).Re.sqrt;
      if (norma>max) {max=norma;}
    }
    max;
  };

  override def toString :String = {
    var resultado :String="(";
    var i :Int = 0;
    for(x <- toList) {
      resultado=resultado+x;
      if (i!=dim-1) {resultado=resultado+",";}
      i=i+1;
    }
    resultado+")";
  }

}

object Kn {
    def apply[T <: Cuerpo[T]](plugin :Almacen1D[T]) = new Kn[T](plugin);
    def apply[T <: Cuerpo[T]](a :Kn[T]) :Kn[T]= Kn(a.plugin.clona);
    def apply[T <: Cuerpo[T]](a :T*)(implicit cero :T) :Kn[T]= Kn(AlmacenDensoVec(a :_*));

    implicit def seq2Vec   [T <:Cuerpo[T]](a :Seq[T])(implicit cero :T)   :Kn[T] = Kn(a :_*);
    implicit def uno2Vec   [T <:Cuerpo[T]](a :T)(implicit cero:T)         :Kn[T] = Kn(a);

    /*def carga[T <:Cuerpo[T]](fichero :String,funcion :String=>T) : Kn[T]= {
      var fic = new BufferedReader(new FileReader(fichero));
      var st =new java.util.StringTokenizer(fic.readLine);
      var vec :Kn[T]= new Kn[T](st.countTokens);
      for(i<-0 until st.countTokens) {
        vec(i)=funcion(st.nextToken);
      }
      fic.close;
      vec;
    }
    def cargaLista[T <:Cuerpo[T]](fichero :String,funcion :String=>T) : Seq[Kn[T]]= {
      var fic = new BufferedReader(new FileReader(fichero));
      var salida :List[Kn[T]]=Nil;
      var linea=fic.readLine;
      while(linea!=null) {
          var st=new java.util.StringTokenizer(linea);
          var vec = new Kn[T](st.countTokens);
          for(i<-0 until st.countTokens) {
              vec(i)=funcion(st.nextToken);
          }
          salida=vec::salida;
          linea=fic.readLine;
      };
      fic.close;
      salida.reverse;
    }
    def write[T <:Cuerpo[T]](fichero :String,entrada :Seq[Kn[T]]) {
        var fic = new BufferedWriter(new FileWriter(fichero));
        for(i<-entrada) {
            fic.write(i(0).toString+" "+i(1).toString);
            fic.newLine;
        }
        fic.close;
    }
    */
}
