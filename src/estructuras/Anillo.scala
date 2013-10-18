package estructuras

import utilidades.Consola

trait Anillo[T <: Anillo[T]] extends GrupoAditivo[T]{

  def uno :T;
  def esUno :Boolean;

  def -(that :T) :T;
  def *(that: T) :T;

  def xprint(c :Consola) {c.xprint(toString);}
  def xprintln(c :Consola) {xprint(c); c.xprintln;};
}