package estructuras

trait GrupoAditivo[T <: GrupoAditivo[T]]{
  
  def get :T;
  
  def cero :T;
  def esCero :Boolean;
  
  def +(that: T) :T;
  def unary_- :T;  
  
  def *(exponente :Int) = {
    var resultado = cero;    
    for(i<-1 to exponente) resultado=resultado + get;
    resultado;
  }   
  
}
