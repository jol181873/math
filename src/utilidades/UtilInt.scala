package utilidades

/*
 * UtilEntero.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

object UtilInt {
    def mcd(a :Int,b :Int) :Int= {
        val resto=a%b;
        if (resto==0) {Math.abs(b);}
        else {mcd(b,resto);}
    }
}
