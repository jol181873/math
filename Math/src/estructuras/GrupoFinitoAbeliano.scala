package estructuras

trait GrupoFinitoAbeliano[T <: GrupoFinitoAbeliano[T]] extends GrupoAditivo[T]{
		
  def ordenAditivo :Int = {
      var resultado=1   
      var valor=get;
    
      while(!valor.esCero) {
        resultado=resultado+1;
        valor=valor + get;      
      }
      resultado;
    }
  
	def generaSubgrupoAditivo :List[T] = {
	  var lista = List[T]();
	  for(i<-1 to ordenAditivo) lista=(get * i)::lista;
	  lista;
	}
	
	def generaSubgrupoAditivo(elemento2 :T) :List[T]={
	  var subgrupo1=generaSubgrupoAditivo;
	  var subgrupo2=elemento2.generaSubgrupoAditivo;
	  var resultado=List[T]();
	  
	  for(i<-subgrupo1) 
	    for(j<-subgrupo2) resultado=(i + j)::resultado;
	  
	  resultado;
	}
}

object GrupoFinitoAbeliano {  
    
}