package estructuras.vector

/*
 * R3.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import numeros._;

class R3(x :Real,y :Real,z :Real) extends Kn(AlmacenDensoVec(x,y,z)){

    def getX = this(1);
    def getY = this(2);
    def getZ = this(3);

    //producto vectorial
    def **(that :R3)       :R3 = new R3(getY*that.getZ-getZ*that.getY,getZ*that.getX-getX*that.getZ,getX*that.getY-getY*that.getX);

    //giro por un cuaternio
    def *(that :Cuaternio) :R3 = that*this;
}

object R3 {
    def apply(x :Real,y :Real,z :Real) = new R3(x,y,z);

    implicit def tupla2R3(a :(Real,Real,Real))      :R3         = R3(a._1,a._2,a._3);
    implicit def kntoR3  (x :Kn[Real])              :R3         = R3(x(1),x(2),x(3));
    implicit def listKntoListR3(l :List[Kn[Real]])  :List[R3]   = l.map(a=>new R3(a(1),a(2),a(3)));
}
