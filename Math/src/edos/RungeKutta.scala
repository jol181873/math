package edos

/*
 * RungeKutta.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import java.io._;

import numeros._;
import estructuras._;
import matriz._;
import vector._;
import funciones._;

class RungeKutta(fichero :String) {
    /*var directorio = new File(fichero).getParent;
    var fic = new BufferedReader(new FileReader(fichero));
    var a :Matriz[Real]=Matriz.carga(directorio+"/"+fic.readLine,x=>new Real(x.toDouble),Real.Cero);
    var c :Kn[Real]=Kn.carga(directorio+"/"+fic.readLine,x=>new Real(x.toDouble));
    var b :Kn[Real]=Kn.carga(directorio+"/"+fic.readLine,x=>new Real(x.toDouble));
    var be:Kn[Real]=Kn.carga(directorio+"/"+fic.readLine,x=>new Real(x.toDouble));

    def resuelve(funciones :List[String],errorabs :Double,x0 :Double,xf :Double,y0 :Kn[Real]) :Kn[Real] = {
       val dimension=a.m;
       val errorrel=errorabs/10;
       var xn = x0;
       var ymax = y0;
       var numero_aceptados=0;
       var numero_fallados=0;
       val fcn =new Funcion(funciones);
       var h=Math.pow(errorabs+errorrel*ymax.norma2/fcn.calc(xn,ymax).norma2,1/5);

        while (xn<xf) {
             for (i<-0 until dimension) {
             }
             var ytemp :Kn[Real] = ymax//ymax+h*(G*!b);
             var estimacion :Double = 0.0 //h*(G*!(b-be)).norma;
             var tolerancia :Double=errorabs+errorrel*ytemp.norma2;
             var factor :Double=0.0;

             if (estimacion<=tolerancia) {
                xn=xn+h;
                numero_aceptados=numero_aceptados+1;
                ymax=ytemp;
                factor=Math.pow(0.9*tolerancia/estimacion,1/5);
                factor=Math.min(2,factor);
             } else {
                numero_fallados=numero_fallados+1;
                factor=Math.pow(0.9*tolerancia/estimacion,1/5);
                factor=Math.max(1/10,factor);
             }
             h=factor*h;
             if (xn+h>xf) h=xf-xn;
        }
        ymax;
    } */
}
