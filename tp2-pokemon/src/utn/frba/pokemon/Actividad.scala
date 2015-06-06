package utn.frba.pokemon

class ResultadoActividad(val puedeEjecutar : Boolean, val descripcion : String = "") {

}

abstract class Actividad {
   def puedeEjecutar(estado : Estado) : Boolean = {
      estado match {
        case EstadoKO => false
      }
   }
   
   def ejecutar(pokemon : Pokemon) : ResultadoActividad = {
     if (puedeEjecutar(pokemon.estado))
       new ResultadoActividad(puedeEjecutar=true)
     else
       new ResultadoActividad(puedeEjecutar=false, descripcion="Estado incorrecto")
  }
}


class RealizarUnAtaque(ataque : Ataque) extends Actividad{

  override def ejecutar(pokemon : Pokemon) : ResultadoActividad = {
    var resultado = super.ejecutar(pokemon)
    
    if (resultado.puedeEjecutar) {
      //TODO
    }
      
    resultado 
    
  }
}