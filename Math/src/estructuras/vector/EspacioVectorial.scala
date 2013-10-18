package estructuras.vector

/*
 * Modulo.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import estructuras._;

trait EspacioVectorial[T <:Cuerpo[T],S <:EspacioVectorial[T,S]] {

  def get :S;

  def +(that :S) :S;
  def -(that :S) :S;
  def unary_-    :S;
  def *(that: T) :S; //operacion externa

  def dim :Int;
  def base :List[S];
}
