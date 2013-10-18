package test

import numeros.{EnteroModular,Entero}
import numeros.EnteroModular._;
import estructuras._;
import estructuras.matriz._;
import java.util.Scanner

/**
 * Created by IntelliJ IDEA.
 * User: jol
 * Date: 26-mar-2010
 * Time: 12:23:25
 * To change this template use File | Settings | File Templates.
 */

object TestMatrizModular {
 def main(ars :Array[String]) :Unit = {
   
   modulo=26;
   
   var matrizModular = Matriz(AlmacenHueco[EnteroModular](2,2,13,7,8,21));
   
   println("Modulo: "+modulo);
   println(matrizModular);
   println("det:"+matrizModular.det);   
   println("det inverso:"+matrizModular.det.inverso);
   var inversa=matrizModular.inversaDet;
   println(inversa);
   println(matrizModular*inversa);
      
   val sc=new Scanner(System.in);
   println("Introduzca el módulo: ");
   modulo=sc.nextInt();
   
   println("Sea Z"+modulo);  
   println("¿Es un cuerpo? "+EnteroModular.esCuerpo());
   println("Sus unidades son:"+EnteroModular.unidades());
   println("Sus no unidades son:"+EnteroModular.noUnidades());
      
   println("Introduzca el elemento: ");
   val elemento :Entero=sc.nextInt();
   println("Orden aditivo: "+elemento.ordenAditivo);
   
   println("El grupo generado por ese elemento es:");
   if (elemento.esUnidad) println("El total porque es unidad");
   var grupo=elemento.generaSubgrupoAditivo;
   println(grupo); 
   
   println("Orden multiplicativo: "+elemento.ordenMultiplicativo);
   var grupo2=elemento.generaSubgrupoMultiplicativo;
   println(grupo2);

 }
}