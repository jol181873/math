package utilidades

/*
 * Consola.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import numeros._;
import estructuras._;
import scala.collection.mutable.Stack;

class Consola(deltax :Int,deltay :Int) {
    private var arr = Array.ofDim[Char](deltax,deltay);
    clrscr;
    var x=0;
    var y=0;
    private var s :Stack[(Int,Int)]=new Stack();

    def apply(x :Int,y :Int) :Char = arr(x)(y);
    def update(x :Int,y :Int,c :Char) {arr(x)(y)=c;}
    def gotoxy(x :Int,y :Int) {this.x=x;this.y=y;}
    def inc(deltax :Int,deltay :Int) {this.x=this.x+deltax;this.y=this.y+deltay;}
    def push {s.push((x,y))}
    def pop {
        val n=s.pop;
        x=n._1;
        y=n._2;}
    def top {
        val n=s.top;
        x=n._1;
        y=n._2;
    }

    def clrscr {
      for(i<-0 until deltax) for(j<-0 until deltay) arr(i)(j)=' ';
      x=0;
      y=0;
    }

    def xprint(x :Int,y :Int,s :String) {
        var j=0;
        for(i<-s) {
            this(x+j,y)=i;
           j=j+1;
        }
        inc(j+1,0);
    }
    def xprint(s :String) {xprint(x,y,s);}
    def xprintln {gotoxy(0,y+1);}


    def imprime(deltax :Int,deltay :Int) {
        for(i<-0 to deltay)
        {
            var s :String="";
            for(j<-0 to deltax) s=s+this(j,i);
            println(s);
        }
    }
}

object Consola extends Consola(200,200);
