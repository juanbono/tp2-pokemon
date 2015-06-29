package utn.frba.pokemon
import scala.util.{Try, Success, Failure}


/*
 * Una Activdad va de un pokemon y devuelve un Estado de resultado, conteniendo el pokemon modificado si corresponde.
 */
trait Actividad {
   def aplicar(pokemon : Pokemon) : Try[Pokemon]
   
   def ejecutar(pokemon : Pokemon) : Try[Pokemon] = {
     
     pokemon.estado match {
       case EstadoKO(_) => Try(throw KOException("El pokemon no puede realizar la actividad porque esta KO"))
       case EstadoDormido(1) => Try(pokemon.cambiarEstado(EstadoNormal))
       case EstadoDormido(x) => Try(pokemon.cambiarEstado(EstadoDormido(x - 1)))
       case _ => aplicar(pokemon)
     }
   }
  }

case class RealizarUnAtaque(ataque: Ataque) extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = {
    ataque.aplicar(pokemon)
  }
}

case class LevantarPesas(kilos: Int) extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = {
    var experiencia = 0
    
    pokemon match {
      case _ if pokemon.estado == EstadoParalizado => Try(pokemon.cambiarEstado(EstadoKO("Pokemon paralizado intento levantar pesas"))) 
      case _ if pokemon.algunTipoEs(Fantasma) => Try(throw InvalidPokemonTypeException("Los pokemon de tipo fantasma no pueden levantar pesas."))
      case _ if kilos / pokemon.fuerza > 10 => Try(pokemon.cambiarEstado(EstadoParalizado))
      case _ if pokemon.algunTipoEs(Pelea) => experiencia = kilos * 2
      case _ => experiencia = kilos
    }
    
    Try(pokemon.cambiarExperiencia(experiencia))
  }
}

case class Nadar(minutos: Int) extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = {
    
    pokemon match {
      case _ if (Agua.mataA(pokemon.tipoPrincipal) || Agua.mataA(pokemon.tipoSecundario.getOrElse(Agua))) =>  Try(pokemon.cambiarEstado(EstadoKO("El pokemon pierde contra el agua")))
      case _ => Try(pokemon.cambiarExperiencia(200).cambiarEnergia(-minutos).cambiarVelocidad(if (pokemon.tipoPrincipal == Agua) minutos % 60 else 0))
    }
    
  }
} 

case class AprenderAtaque(ataque: Ataque) extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = { 
    pokemon match {
      case _ if pokemon.algunTipoEs(ataque.tipo) || ataque.tipo == Normal => Try(pokemon.copy(ataques = ataque.copy(puntosDeAtaque = ataque.maximoInicialPA) :: pokemon.ataques))
      case _ => Try(pokemon.cambiarEstado(EstadoKO("Pokemon se lastimo trantando de aprender ataque")))
    }

  }
}

case class UsarPiedra(piedra: PiedraEvolutiva) extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = { 
    pokemon match {
       case _ if piedra.tipo.mataA(pokemon.tipoPrincipal) || piedra.tipo.mataA(pokemon.tipoSecundario.getOrElse(null)) =>  Try(pokemon.cambiarEstado(EstadoEnvenenado))
       case _ => Try(pokemon.especie.condicionEvolucion.fold(pokemon)(_.evolucionar(pokemon, piedra)))
    }
  }
}

case object UsarPocion extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = Try(pokemon.cambiarEnergia(50))
}

case object UsarAntidoto extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = {
   pokemon.estado match {
     case EstadoEnvenenado => Try(pokemon.cambiarEstado(EstadoNormal))
     case _ => Try(pokemon)
   } 
  }
}

case object UsarEther extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = {
   pokemon.estado match {
     case EstadoKO(_) => Try(pokemon)
     case _ => Try(pokemon.cambiarEstado(EstadoNormal))
   } 
  }
}

case object ComerHierro extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = Try(pokemon.cambiarFuerza(5))
}

case object ComerCalcio extends Actividad {
   def aplicar(pokemon : Pokemon) : Try[Pokemon] = Try(pokemon.cambiarVelocidad(5))
}

case object ComerZinc extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = {
   val ataquesModificados = pokemon.ataques.map (ataque => ataque.copy(maximoInicialPA = ataque.maximoInicialPA + 2))
   Try(pokemon.copy(ataques = ataquesModificados))
  }
}

case object Descansar extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = {  
   val ataquesModificados = pokemon.ataques.map (ataque => ataque.copy(puntosDeAtaque = ataque.maximoInicialPA))
   val pokemonResultado = pokemon.copy(ataques = ataquesModificados)
   pokemon.estado match {
     case EstadoNormal => if (pokemon.energia < pokemon.energiaMaxima / 2) Try(pokemonResultado.cambiarEstado(EstadoDormido(3))) else Try(pokemonResultado)
     case _            => Try(pokemonResultado)
   }
  }
}

case object FingirIntercambio extends Actividad {
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = {
    val nuevoPokemon = pokemon.especie.condicionEvolucion.fold(pokemon)(_.evolucionar(pokemon))
    
    if (nuevoPokemon.esHembra)
      Try(nuevoPokemon.cambiarPeso(-10))
    else
      Try(nuevoPokemon.cambiarPeso(1))
  }
}