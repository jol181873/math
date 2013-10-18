package estructuras

trait GrupoFinitoNoAbeliano[T <: GrupoFinitoNoAbeliano[T]] extends GrupoMultiplicativo[T]{

    def ordenMultiplicativo :Int = {
	  var resultado=1   
      var valor=get;
	    
	  while(!valor.esUno) {
		  resultado=resultado+1;
	      valor=valor * get;      
	  }
	  resultado;
	}
  
  	def generaSubgrupoMultiplicativo :List[T] = {
	  var lista = List[T]();
	  for(i<-1 to ordenMultiplicativo) lista=(get ^ i)::lista;
	  lista;
	}
	
	def generaSubgrupo(elemento2 :T) :List[T]={
	  var subgrupo1=generaSubgrupoMultiplicativo;
	  var subgrupo2=elemento2.generaSubgrupoMultiplicativo;
	  var resultado=List[T]();
	  
	  for(i<-subgrupo1) 
	    for(j<-subgrupo2) resultado=(i * j)::resultado;
	  
	  resultado;
	}

}

object GrupoFinitoNoAbeliano {
  

}