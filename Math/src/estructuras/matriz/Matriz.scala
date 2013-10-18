package estructuras.matriz

/*
 * Matriz.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import java.io._;
import scala.actors._;
import estructuras._;
import vector._;
import excepciones._;
import numeros._
import util.Random
;

class Matriz[T <: Cuerpo[T]](plugin :Almacen2D[T]) extends CuasiMatriz[T](plugin){

    def *(x :Kn[T]) :Kn[T] = {
    	if (x.dim!=n) throw new DimensionException();
    	val salida=this * Matriz[T](plugin.get(x.dim,1,x.toList));
    	salida.columna(1);
    }

    def columna(j :Int) :Kn[T]= {
        val salida=Kn(AlmacenDensoVec(m));
        for(i<-1 to m) salida(i)=this(i,j);
        salida;
    }

    def inversaDet = {
        if (m!=n) throw new DimensionException();
        val salida = Matriz[T](plugin.get(m,n));
        var d=cero;
        for (i<-1 to m) {
            for(j<-1 to n) {
                salida(i,j)=subMatriz(j,i).det;
                if ((i+j)%2==1) salida(i,j)= -salida(i,j);
            }
        }
        for (i<-1 to n) {
            d=d+this(1,i)*salida(i,1);
        }
        /*if (d.esCero) throw new MatrizSingularException();*/
        try{
          salida*d.inverso;
        } catch {
          case ex :NoTieneInversoException => throw new MatrizSingularException();
        }

    }
    def escalona :(Matriz[T],Matriz[T]) = {
        var salida = Matriz.identidadHueca[T](m,n);
        var entrada = Matriz(this);
        for(j<-1 to m) {
            for(i<-j to m-1) {
                salida. combinaFila(i+1,j,-entrada(j,j).inverso*entrada(i+1,j));
                entrada.combinaFila(i+1,j,-entrada(j,j).inverso*entrada(i+1,j));
            }
        }
        (entrada,salida);
    }

    def inversa = {
        if (m!=n) throw new DimensionException();
        var (entrada,salida)=escalona;
        for(j<-1 to m) {
            for(i<-j to m-1) {
                salida. combinaFila(m-i,m-j+1,-entrada(m-j+1,m-j+1).inverso*entrada(m-i,m-j+1));
                entrada.combinaFila(m-i,m-j+1,-entrada(m-j+1,m-j+1).inverso*entrada(m-i,m-j+1));
            }
        }
        for(i<-1 to m)  if (entrada(i,i).esCero) throw new MatrizSingularException()
                        else salida.multiplicaFila(i,entrada(i,i).inverso);
        salida;
    }

    def det2 = {
        if (m!=n) throw new DimensionException();
        var (entrada,salida) = escalona;
        var det=this(1,1).uno;
        for (i<-1 to m) det=det*entrada(i,i);
        det;
    }

    def conjugada = Matriz[T](plugin.get(m,n,toList.map(_.conjugado)));

    def polChar :Polinomio[T] = {
        if (!esCuadrada) throw new DimensionException();
        var mat = CuasiMatriz[Polinomio[T]](new AlmacenDenso[Polinomio[T]](m,n,Polinomio(cero).cero));
        for (i<-1 to m) {
            for (j<-1 to n) {
                if (i==j) mat(i,j)=Polinomio(this(i,j),-this(i,j).uno)
                else mat(i,j)=Polinomio(this(i,j));
            }
        }
        mat.det;
    }

}

object Matriz {
    def apply[T <: Cuerpo[T]](plugin :Almacen2D[T]) :Matriz[T] = new Matriz(plugin);
    def apply[T <: Cuerpo[T]](a :CuasiMatriz[T]) :Matriz[T] = Matriz(a.plugin.clona)
    def apply[T <: Cuerpo[T]](m :Int,n :Int, a :T*)(implicit cero :T) :Matriz[T] = Matriz(AlmacenDenso(m,n,a :_*));
  
    //array de vectores columna
    def apply[T <: Cuerpo[T]](plugin :Almacen2D[T],a :Array[Kn[T]]) :Matriz[T] = {
      val salida=Matriz(plugin.get(a(0).dim,a.length));
      for (i<-1 to plugin.m) {
          for (j<-1 to plugin.n) salida(i,j)=a(j-1)(i);
      }
      salida;
    }

    def identidad[T <: Cuerpo[T]](m :Int,n :Int)(implicit cero :T) = Matriz(AlmacenIdentidad[T](m,n));
    def identidadHueca[T <: Cuerpo[T]](m :Int,n :Int)(implicit cero :T) = Matriz(AlmacenIdentidad[T](m,n).clona);


    //Ortogonalizacion mediante el algoritmo de Gram-Smith
	  //si la matriz es cuadrada entonces Q es ortogonal
    //R es triangular superior cumpliendose A=QR
	  //Las columnas de Q son ortonormales
    def QR(a :Matriz[Real]) :(Matriz[Real],Matriz[Real]) = {
        var col = Array.ofDim[Kn[Real]](a.n,a.n);
        var q =new Array[Kn[Real]](a.n);
        var matrizR = Matriz.identidadHueca[Real](a.n, a.n);

        for(i<-0 until a.n) col(i)(0)=a.columna(i+1);

        for(i<-0 until a.n) {
            q(i)=col(i)(i);
            q(i)=q(i)*(q(i).norma2.inverso);
			      for(j<-i+1 until a.n){
				        var proyeccion=q(i)*col(j)(i);

				        matrizR(i+1,j+1)=proyeccion;
				        col(j)(i+1)=col(j)(i)-q(i)*proyeccion;
            }
			      matrizR(i+1,i+1)=col(i)(i).norma2;
        }
        (Matriz(a.plugin,q),matrizR);
    }

    //Calcula los valores propios reales mediante el algoritmo QR
    def valores_propios(that :Matriz[Real],iteraciones :Int) :Array[Real] = {
        var a = Matriz(that);
        var salida=new Array[Real](that.m);
		    for(i<-1 to iteraciones) {
			      var qr=Matriz.QR(a);
			      a=qr._2*qr._1;			     
        }
        for (i<-1 to that.m) salida(i-1)=a(i,i);
        salida;
    }

    //resuelve el sistema Ax=b si A es definida positiva y simetrica
    //mediante el algoritmo del Descenso Maximo, imax es el número máximo de iteraciones
    def resuelveMaximoDescenso(imax :Int,epsilon :Real)(a :Matriz[Real],b :Kn[Real]) :(Int,Kn[Real]) = {
    	var x = Kn(b);
    	var i=0;
    	var r=b-a*x;
    	var delta=r*r;
    	var parada=epsilon.sqr*delta;
    	while (i<imax && delta>parada) {
    		var q=a*r;
    		var alpha=delta/(r*q);
    		x=x+r*alpha;
    		if (i%50==0) r=b-a*x;
    		else r=r-q*alpha;
    		delta=r*r;
    		i=i+1;
    	}
    	(i,x);
    }

    //resuelve el sistema Ax=b si A es definida positiva y simetrica
    //mediante el algoritmo del Gradiente Conjugado que converge en dim(b) pasos a la solución
    def resuelveGradienteConjugado(a :Matriz[Real],b :Kn[Real]) :(Int,Kn[Real]) = {
    	var x = Kn(b);
    	var r=b-a*x;
    	var d=r;
    	var delta=r*r;
    	var j=0;
    	for(i<-0 to b.dim*b.dim if !delta.esCero) {
    		var q=a*d;
    		var alpha=delta/(d*q);
    		x=x+d*alpha;
    		if (i%50==0) r=b-a*x;
    		else r=r-q*alpha;
    		var deltaviejo=delta;
    		delta=r*r;
    		var beta=delta/deltaviejo;
    		d=r+d*beta;
    		j=i;
    	}
    	(j,x);
    }

    //funcion auxiliar para Jacobi y Gauss-Seidel
    private def resuelveJG(imax :Int,epsilon :Real,b :Kn[Real],d :Matriz[Real],r :Matriz[Real]) :(Int,Kn[Real])={
    	var c=d*b;
    	var g=(d*(-1.0))*r;

    	var x0=b;
    	var x=g*x0+c;
    	var i=0;
    	while(i<imax && (x-x0).norma2>epsilon) {
    		x0=x;
    		x =g*x+c;
    		i=i+1;
    	}
    	(i,x);
    }
    //resuelve el sistema Ax=b
    //mediante el algoritmo de Jacobi que converge si A es estrictamente diagonal dominante
    //o si y solo si el radio espectral de A es menor que 1
    def resuelveJacobi(imax :Int,epsilon :Real)(a :Matriz[Real],b :Kn[Real]) :(Int,Kn[Real]) = {
    	var r = Matriz(a.plugin.get(a.m,a.n));
    	var d = Matriz(a.plugin.get(a.m,a.n));
    	for (i<-1 to a.m)
    		for (j<-1 to a.n) if (i==j) d(i,j)=a(i,j).inverso;
    			else r(i,j)=a(i,j);
    	resuelveJG(imax,epsilon,b,d,r);
    }

    //resuelve el sistema Ax=b
    //mediante el algoritmo de Gauss-Seidel que converge si A es simetrica y definida positiva
    //o si y solo si el radio espectral de A es menor que 1
    //requiere el calculo de la inversa de una matriz
    def resuelveGaussSeidel(imax :Int,epsilon :Real)(a :Matriz[Real],b :Kn[Real]) :(Int,Kn[Real]) = {
    	var r = Matriz(a.plugin.get(a.m,a.n));
    	var d = Matriz(a.plugin.get(a.m,a.n));
    	for (i<-1 to a.m)
    		for (j<-1 to a.n) if (i<=j) d(i,j)=a(i,j);
    			else r(i,j)=a(i,j);

    	var q=d.inversa;
    	resuelveJG(imax,epsilon,b,q,r);
    }

    def resuelve[T <:Cuerpo[T]](a :Matriz[T],b :Matriz[T],func :(Matriz[T],Kn[T]) => (Int,Kn[T])) :Matriz[T] ={
      def run(b :Kn[T],j :Int) :(Kn[T],Int) = {
        val salida = func(a, b);
        (salida._2,j);
      }

      val salida = new Array[Kn[T]](b.n);
      val fut = for (j <- 1 to b.n) yield Futures.future{run(b.columna(j), j - 1)}
      for(f<-fut) {
        val resultado = f();
        salida(resultado._2)=resultado._1;
      }
      Matriz[T](a.plugin,salida);
    }

    def inversaGrad   		(a :Matriz[Real]) 							            :Matriz[Real] = Matriz.resuelve(a,Matriz.identidad(a.m,a.n)(a.cero),resuelveGradienteConjugado);
    def inversaMaxDesc		(a :Matriz[Real],imax :Int,epsilon :Real) 	:Matriz[Real] = Matriz.resuelve(a,Matriz.identidad(a.m,a.n)(a.cero),resuelveMaximoDescenso(imax,epsilon));
    def inversaJacobi 		(a :Matriz[Real],imax :Int,epsilon :Real) 	:Matriz[Real] = Matriz.resuelve(a,Matriz.identidad(a.m,a.n)(a.cero),resuelveJacobi(imax,epsilon));
    def inversaGaussSeidel(a :Matriz[Real],imax :Int,epsilon :Real) 	:Matriz[Real] = Matriz.resuelve(a,Matriz.identidad(a.m,a.n)(a.cero),resuelveGaussSeidel(imax,epsilon));


    def carga[T <:Cuerpo[T]](fichero :String,funcion :String=>T,cero :T) :Matriz[T]= {
        /*var fic = new BufferedReader(new FileReader(fichero));
        var salida = new Matriz[T](fic.readLine.toInt,fic.readLine.toInt,cero);
        var linea :String=null;
        var fila=1;
        do {
            linea=fic.readLine;
            if (linea!=null){
                var st =new java.util.StringTokenizer(linea);
                for(i<-1 to st.countTokens) {
                    salida(fila,i)=funcion(st.nextToken);
                }
            }
            fila=fila+1;
        } while(linea!=null);
        fic.close;
        salida;*/
      null;
    }

    def defPositiva(m :Int, n :Int,min :Real,max :Real) :CuasiMatriz[Real]= {
      val a=AlmacenDenso[Real](m,n).fillTriangularInf(()=>Real.randomRealPositivo(min,max));
      val b=Matriz(a);
      b * !b;
    }
}

