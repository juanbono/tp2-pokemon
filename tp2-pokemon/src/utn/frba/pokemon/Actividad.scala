package utn.frba.pokemon

/*
 * Una Activdad va de un pokemon y devuelve un Estado de resultado, conteniendo el pokemon modificado si corresponde.
 */
trait Actividad {
   def aplicar(pokemon : Pokemon) : Estado
   
   def ejecutar(pokemon : Pokemon) : Estado = {
     validarActividad(pokemon, aplicar(pokemon))
   }
   
   def validarActividad(pokemonOriginal : Pokemon, estado : Estado) : Estado = {
     var mensaje : List[String] = List()
     val pokemon = estado.pokemon
     
     estado.pokemon match {
       case _ if pokemon.esPokemonValido       => return estado
       case _ if !pokemon.esNivelValido        => mensaje = "Nivel debe ser un numero de 1 a 100" :: mensaje 
       case _ if !pokemon.esGeneroValido       => mensaje = "Genero debe ser Masculino o Femenino" :: mensaje  
       case _ if !pokemon.esFuerzaValida       => mensaje = "Fuerza debe ser un numero de 1 a 100" :: mensaje 
       case _ if !pokemon.esVelocidadValida    => mensaje = "velocidad debe ser un numero de 1 a 100" :: mensaje   
       case _ if !pokemon.esPesoValido         => mensaje = "peso debe ser un numero de 1 a 100" :: mensaje
     }

       EstadoActividadNoEjecutada(pokemonOriginal, ("La actividad produce estado invalido" :: mensaje).toString())
   }
}

case class RealizarUnAtaque(ataque: Ataque) extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = {
    ataque.ejecutar(pokemon)
  }
}

case class LevantarPesas(kilos: Int) extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = {
    var experiencia = 0
    
    pokemon match {
      case _ if pokemon.algunTipoEs(Fantasma) => return EstadoActividadNoEjecutada(pokemon, "El tipo fantasma no puede levantar pesas")
      case _ if kilos / pokemon.fuerza > 10 => return EstadoParalizado(pokemon)
      case _ if pokemon.algunTipoEs(Pelea) => experiencia = kilos * 2
      case _ => experiencia = kilos
    }
    
    EstadoNormal(pokemon.subirExperiencia(experiencia))
  }
}

case class Nadar(minutos: Int) extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = {
    
    pokemon match {
      case _ if (Agua.mataA(pokemon.tipoPrincipal) || Agua.mataA(pokemon.tipoSecundario.getOrElse(Agua))) =>  EstadoKO(pokemon, "El pokemon pierde contra el agua")
      case _ => EstadoNormal(pokemon.subirExperiencia(200).bajarEnergia(minutos).subirVelocidad(if (pokemon.tipoPrincipal == Agua) minutos % 60 else 0))
    }
    
  }
} 

case class AprenderAtaque(ataque: Ataque) extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = { 
    pokemon match {
      case _ if pokemon.algunTipoEs(ataque.tipo) || ataque.tipo == Normal => EstadoNormal(pokemon.copy(ataques = ataque.copy(puntosDeAtaque = ataque.maximoInicialPA) :: pokemon.ataques))
      case _ => EstadoKO(pokemon, "Pokemon se lastimo trantando de aprender ataque")
    }

  }
}

case class UsarPiedra(piedra: PiedraEvolutiva) extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = { 
    pokemon match {
       case _ if piedra.tipo.mataA(pokemon.tipoPrincipal) || piedra.tipo.mataA(pokemon.tipoSecundario.getOrElse(null)) =>  EstadoEnvenenado(pokemon)
       case _ => EstadoNormal(pokemon.especie.condicionEvolucion.fold(pokemon)(_.usarPiedra(pokemon, piedra)))
    }
  }
}

case object UsarPocion extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon.subirEnergia(50))
}

case object UsarAntidoto extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon)
}

case object UsarEther extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon)
}

case object ComerHierro extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon.subirFuerza(5))
}

case object ComerCalcio extends Actividad {
   def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon.subirVelocidad(5))
}

case object ComerZinc extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = {
   val ataquesModificados = pokemon.ataques.map (ataque => ataque.copy(maximoInicialPA = ataque.maximoInicialPA + 2))
   EstadoNormal(pokemon.copy(ataques = ataquesModificados))
  }
}

case object Descansar extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = {  
   val ataquesModificados = pokemon.ataques.map (ataque => ataque.copy(puntosDeAtaque = ataque.maximoInicialPA))
   val pokemonResultado = pokemon.copy(ataques = ataquesModificados)
   if (pokemon.energia < pokemon.energiaMaxima / 2) EstadoDormido(pokemonResultado) else EstadoNormal(pokemonResultado)
  }
}

case object FingirIntercambio extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = {
    val nuevoPokemon = pokemon.especie.condicionEvolucion.fold(pokemon)(_.intercambiar(pokemon))
    
    if (nuevoPokemon.esHembra)
      EstadoNormal(nuevoPokemon.bajarPeso(10))
    else
      EstadoNormal(nuevoPokemon.subirPeso(1))
  }
}