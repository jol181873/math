package aproximacion

import numeros.Real
import estructuras.vector.Kn

/**
 * Created by IntelliJ IDEA.
 * User: jol
 * Date: 24-dic-2009
 * Time: 19:30:35
 * To change this template use File | Settings | File Templates.
 */

object BSpline {
  /**
   *  Genera los nudos necesarios para una curva B-spline de grado k y n+1 puntos de control
   */
  def generaNudos(n :Int,k :Int,a :Real,b :Real) :(Int,Seq[Real]) ={
    var tau = new Array[Real](n+k+2);
    for(i<-(-k-1) to -1) tau(i+k+1)=a;
    for(i<-0 to n-k-1) tau(i+k+1)=a+(i+1)*(b-a)/(n-k+1);
    for(i<-n-k to n) tau(i+k+1)=b;
    (k,tau);
  }

  /**
   * Genera los nudos necesarios para una curva B-spline de n+1 puntos de control y grado n+1 que coincide con una curva de Bezier
   */
  def generaNudosBezier(n :Int,a :Real,b :Real) :(Int,Seq[Real]) = generaNudos(n,n,a,b);

  /**
   * Calcula el valor correspondiente a un t dado
   */
  def bspline(t :Real,p :Seq[Kn[Real]],nudos :(Int,Seq[Real])) :Kn[Real] ={
    val tau=nudos._2;
    val k=nudos._1;
    var r :Int = p.length-k-1;
    for(i<-0 to p.length-k-1) if (tau(i+k+1).toDouble>t.toDouble && r == p.length-k-1) r=i;
    var q = new Array[Kn[Real]](p.length);
    p.copyToArray(q,0);

    for(j<-1 to k) {
      for(i<-r to r+k-j) {
        val den=tau(i+k+1)-tau(i+j);
        if (den.esCero) {for (h<-0 until q(i).dim) q(i)(h)=0.0;}
        else {
          val lambda=(t-tau(i+j))/den;
			    q(i)=q(i)*(1-lambda)+q(i+1)*lambda;
        }
      }
    }
    q(r);
  }

  /**
   * Calcula una lista de np puntos de la curva B-spline con los puntos de control dados en p
   */
  def bspline(p :Seq[Kn[Real]],nudos :(Int,Seq[Real]),np :Int) :Seq[Kn[Real]] = {
    val h=1.0/np;
    var salida :List[Kn[Real]]=Nil;
    for(l<-0 to np) {
      var t=l*h;
      salida=bspline(t,p,nudos)::salida;
    }
    salida.reverse;
  }

  /**
   * Calcula una lista de np puntos de la curva B-spline de grado 3 con los puntos de control dados en p
   */
  def bspline(p :Seq[Kn[Real]],np :Int) :Seq[Kn[Real]] = bspline(p,generaNudos(p.length-1,3,0,1),128);

  /**
   * Calcula una lista de np puntos de la curva B-spline de grado adecuado para que resulte una curva de Bezier
   */
  def bezier (p :Seq[Kn[Real]],np :Int) :Seq[Kn[Real]] = bspline(p,generaNudosBezier(p.length-1,0,1),128);

}