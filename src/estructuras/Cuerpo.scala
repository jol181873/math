package estructuras

import numeros.Real;

trait Cuerpo[T <: Cuerpo[T]] extends DominioEuclideo[T] with GrupoMultiplicativo[T]{

  def inverso :T;
  override def %(that :T) :T=cero;
  override def funcionEuclidea = 0;

  def conjugado :T;
  def Re :Real;
 
}
