package estructuras.normado

/*
 * EspacioNormado.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import numeros._;

trait EspacioNormado {

    def norma2        :Real;
    def norma1        :Real = norma2;
    def normaInfinito :Real = norma2;

}
