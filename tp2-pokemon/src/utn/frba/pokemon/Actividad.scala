package utn.frba.pokemon

abstract class Actividad {
   def puedeEjecutar(pokemon : Pokemon) : Boolean = {
      pokemon.estado match {
        case EstadoKO => false
        case _ => true
      }
   }
   
   def ejecutar(pokemon : Pokemon) : ResultadoActividad
}


class RealizarUnAtaque(ataque : Ataque) extends Actividad{
  
  override def puedeEjecutar(pokemon : Pokemon) : Boolean = {
    super.puedeEjecutar(pokemon)// && pokemon.ataques.contains(ataque) && ataque.puntosDeAtaque > 0
  }
  
  override def ejecutar(pokemon : Pokemon) : ResultadoActividad = {
    var resultado = new ResultadoActividad(None, puedeEjecutar(pokemon))
    var pokemonResultado : Pokemon = pokemon
    
    if (resultado.puedeEjecutar) { 
      if (ataque.tipo == Dragon)
        pokemonResultado = pokemonResultado.copy(experiencia = pokemonResultado.experiencia + 80)
      else if (ataque.tipo == pokemonResultado.especie.tipoPrincipal)
        pokemonResultado = pokemonResultado.copy(experiencia = pokemonResultado.experiencia + 50)
      else if (Some(ataque.tipo) == pokemonResultado.especie.tipoSecundario)
       pokemonResultado = pokemonResultado.copy(experiencia = pokemonResultado.experiencia + (if (pokemonResultado.genero == Masculino) 20 else 40))  
       
      pokemonResultado = ataque.ejecutar(pokemonResultado)
    }

    resultado.copy(pokemon = Some(pokemonResultado.subirNivel))
  }
}

class LevantarPesas(kilos : Int) extends Actividad {
  override def puedeEjecutar(pokemon : Pokemon) : Boolean = {
    var puedeEjecutar = super.puedeEjecutar(pokemon)
    if (pokemon.especie.tipoPrincipal == Fantasma)
      puedeEjecutar = false
      
    puedeEjecutar
  }
  
  override def ejecutar(pokemon : Pokemon) : ResultadoActividad = {
    //  Cuando un Pokémon levanta pesas, gana 1 punto de experiencia por cada kilo levantado.
    //Los Pokémon con Tipo Principal o Secundario Pelea ganan el doble de puntos.
    //Los Pokémon de tipo Fantasma NO PUEDEN levantar pesas.
    //Si un Pokémon levanta más de 10 kilos por cada punto de Fuerza , no gana nada de
    //Experiencia y queda Paralizado .
    //Si un Pokémon Paralizado levanta pesas, no gana nada de Experiencia y queda K.O.
    new ResultadoActividad(None, puedeEjecutar = false)
   }
  

} 