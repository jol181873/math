package estructuras

/*
 * DFU.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

trait DFU[T <: DFU[T]] extends Anillo[T] {

    def mcd(that :T) :T;
    def mcm(that :T) :T;
}
