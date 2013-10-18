package estructuras.vector

/*
 * R2.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import numeros._;

class R2(x :Real,y :Real) extends Kn(AlmacenDensoVec(x,y)){

    def getX = this(0);
    def getY = this(1);

}

object R2 {
    def apply(x :Real,y :Real) = new R2(x,y);

    implicit def tupla2R2(a :(Real,Real))           :R2         = R2(a._1,a._2);
    implicit def kntoR2  (x :Kn[Real])              :R2         = R2(x(0),x(1));
    implicit def listKntoListR2(l :List[Kn[Real]])  :List[R2]   = l.map(a=>new R2(a(0),a(1)));

}

