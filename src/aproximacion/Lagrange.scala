package aproximacion

import estructuras.{Cuerpo, Polinomio}

/**
 * Created by IntelliJ IDEA.
 * User: jol
 * Date: 31-dic-2009
 * Time: 10:34:01
 * To change this template use File | Settings | File Templates.
 */

object Lagrange {

  def lagrange[T <:Cuerpo[T]](in :Seq[(T,T)]) :Polinomio[T] = {
    var salida=Polinomio(in(0)._1.cero);
    for(i<-0 until in.length) {
      var temp=Polinomio(in(0)._1.uno);
      for (j<-0 until in.length) if (i!=j) temp=temp*Polinomio[T](-in(j)._1,in(0)._1.uno)*((in(i)._1-in(j)._1)).inverso;
				salida=salida+temp*in(i)._2;
    }
    salida;
  }
}