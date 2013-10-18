package funciones

/*
 * Funcion.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import scala.util.parsing.combinator._
import numeros.Real;
import scala.util.parsing.combinator.Parsers;
import estructuras.vector._;

class Funcion(var funciones :List[String]) extends JavaTokenParsers with RunParser{
    lazy val sumExpr = prodExpr ~ rep("+" ~> prodExpr ^^(d => (x :Double) => x+d) |
                                      "-" ~> prodExpr ^^(d => (x :Double) => x-d)) ^^
                       {case seed ~ fs => fs.foldLeft(seed)((a,f)=>f(a))}
    lazy val prodExpr = factor ~ rep("*" ~> factor ^^(d => (x :Double) => x*d) |
                                     "/" ~> factor ^^(d => (x :Double) => x/d)) ^^
                        {case seed ~ fs => fs.foldLeft(seed)((a,f)=>f(a))}

    lazy val factor :Parser[Double] = floatingPointNumber ^^(_.toDouble) | "(" ~> sumExpr <~ ")" |
                                      ("sin" | "sen") ~ "(" ~> sumExpr <~ ")" 	^^{case x :Double => Math.sin(x)} |
                                      "cos" ~ "(" ~> sumExpr <~ ")"				^^{case x :Double => Math.cos(x)} |
                                      ("tan" | "tg") ~ "(" ~> sumExpr <~ ")"	^^{case x :Double => Math.tan(x)} |
                                      "exp" ~ "(" ~> sumExpr <~ ")"				^^{case x :Double => Math.exp(x)} |
                                      "log" ~ "(" ~> sumExpr <~ ")"				^^{case x :Double => Math.log(x)} |
                                      "sqrt" ~ "(" ~> sumExpr <~ ")"			^^{case x :Double => Math.sqrt(x)} |
                                      "pi"^^^Math.PI |
                                      "e"^^^Math.E |
                                      ident ^^{case s :String => mapa(s)};

    type RootType=Double;

    var mapa = Map[String,Double]();

    def root = sumExpr;

    private def rellena(x :Kn[Real]) = {
        var mapa0 = Map[String,Double]();
        for(i<-0 until x.dim) {
            i match {
                case 0 =>mapa0=mapa0+("x"->x(i));
                case 1 =>mapa0=mapa0+("y"->x(i));
                case 2 =>mapa0=mapa0+("z"->x(i));
                case _ =>
            }
            mapa0=mapa0+("x"+i.toString->x(i));
        }
        mapa0;
    }
    def run :Kn[Real] = {
       var salida =Kn(AlmacenDensoVec[Real](funciones.length));
        for(i<-0 until funciones.length) {
            salida(i)=run(funciones(i)).get;
        }
        salida;
    }

    def calc(x :Kn[Real]) :Kn[Real] = {
        mapa=rellena(x);
        run;
    }
    def calc(t :Double,x :Kn[Real]) :Kn[Real] = {
        mapa=rellena(x)+("t"->t);
        run;
    }
}
