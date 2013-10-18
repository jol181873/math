package estructuras

trait GrupoMultiplicativo[T <:GrupoMultiplicativo[T]] {
  
	  def get :T;
  
	  def uno :T;
	  def esUno :Boolean;
	  
	  def *(that: T) :T;
	  def inverso :T;  
	  
	  def ^(exponente :Int) = {
	    var resultado = uno;    
	    for(i<-1 to exponente) resultado=resultado * get;
	    resultado;
	  }
	  	  
}
