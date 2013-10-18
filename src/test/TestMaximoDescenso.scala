package test

import estructuras.matriz._;
import estructuras.vector._
import numeros._;

object TestMaximoDescenso {
  def main(args : Array[String]) : Unit = {
	  //var A=Matriz(AlmacenDenso[Real](3,3,4.0,0.0,1.0, 0.0,2.0,1.0, 1.0,1.0,3.0));
    var A=Matriz.defPositiva(150,150,500,1000);
    //val valores=Matriz.valores_propios(A, 10)
    var inversa=A.inversa;    
    var b=Kn(AlmacenDensoVec[Real](150,()=>Real.randomRealPositivo(10,100),1000,System.currentTimeMillis()));
    var sol=inversa*b;
	  //var b=Kn[Real](1.0,-2.0,3.0);
	  //println(A);
	  //println("Inversa:");
	  //var C=A.inversaDet;
	  //println(C);
	  //println("Vector b:");
	  //println(b);
	  //println("Solucion con la inversa:");
	  //var sol1=C*b;
	  //println(sol1);

	   var t1=0l;
	   var t2=0l;

	   //print("Valores propios     : ");
	   //valores.foreach((r :Real)=>print(r+", "));
	   var escalonada=A.escalona._1;
	   //println(escalonada);
	   print("¿Es definida positiva? :");
	   var boo=true;
	   for(i<-1 to A.n) {
	     if(escalonada(i,i).esCero || escalonada(i,i).esNegativo) boo=false;
	   }
	   println(boo);
	   println("Solucion con inversa: "+sol);
	   println("A*sol=                "+A*sol);
	  
	   println("b=                    "+b);
	   
	println("=========================================================================");
	  println("Solucion con gradiente conjugado:");
	  t1=System.currentTimeMillis;
	  var sol3=Matriz.resuelveGradienteConjugado(A,b);
	  t2=System.currentTimeMillis;
	  println(sol);
	  println(sol3._2);
	  println("Tiempo:"+(t2-t1));
	  println("Norma 2 de la diferencia entre sol1 y sol3:");
      println(sol3._1);
	  println((A*sol3._2-b).norma2);
	  println((sol-sol3._2).norma2);
	  
    println("=========================================================================");
	  println("Solucion con descenso maximo:");
	  t1=System.currentTimeMillis;
	  var sol2=Matriz.resuelveMaximoDescenso(100000,Real.precision)(A,b);
	  t2=System.currentTimeMillis;
	  println(sol);
	  println(sol2._2);
	  println("Tiempo:"+(t2-t1));
	  println("Norma 2 de la diferencia entre sol1 y sol2:");
	  println(((A*sol2._2)-b).norma2);
	  println((sol2._2-sol).norma2);  

  /*  println("=========================================================================");
	  println("Solucion con Jacobi:");
	  t1=System.currentTimeMillis;
	  var sol4=Matriz.resuelveJacobi(1000,Real.precision)(A,b);
	  t2=System.currentTimeMillis;
	  println(sol);
	  println(sol4._2);
	  println("Tiempo:"+(t2-t1));
	  println("Norma 2 de la diferencia entre sol1 y sol4:");
	  println((A*sol4._2-b).norma2);
	  println((sol-sol4._2).norma2);
*/
    println("=========================================================================");
	  println("Solucion con Gauss-Seidel:");
	  t1=System.currentTimeMillis;
	  var sol5=Matriz.resuelveGaussSeidel(100000,Real.precision)(A,b);
	  t2=System.currentTimeMillis;
	  println(sol);
	  println(sol5._2);
	  println("Tiempo:"+(t2-t1));
	  println("Norma 2 de la diferencia entre sol1 y sol5:");
	  println((A*sol5._2-b).norma2);
	  println((sol-sol5._2).norma2);

  /*  println("=========================================================================");
    println("Inversa:");

    t1=System.currentTimeMillis();
    var C=A.inversa;
    t2=System.currentTimeMillis();
    println(t2-t1);
    println(C);
    
    t1=System.currentTimeMillis();
    var G=Matriz.inversaGaussSeidel(A,1000,Real.precision);
    t2=System.currentTimeMillis();
    println("La inversa calculada mediante Gauss-Seidel es correcta:"+C.equals(G));
    println(t2-t1);
    println(G);
    
    t1=System.currentTimeMillis();
    var D=Matriz.inversaGrad(A);
    t2=System.currentTimeMillis();
    println("La inversa calculada mediante gradiente conjugado es correcta:"+C.equals(D));
    println(t2-t1);
    println(D);
    
    t1=System.currentTimeMillis();
    var E=Matriz.inversaMaxDesc(A,1000,Real.precision);
    t2=System.currentTimeMillis();
    println("La inversa calculada mediante descenso maximo es correcta:"+C.equals(E));
    println(t2-t1);
    println(E);*/
    
    
    
	  /*var F=Matriz.inversaJacobi(A,200,Real.precision);
	  println("La inversa calculada mediante Jacobi es correcta:"+C.equals(F));
	  println(F);*/

  }
}
