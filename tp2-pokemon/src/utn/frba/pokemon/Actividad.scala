package utn.frba.pokemon

/*
 * Una Activdad va de un pokemon y devuelve un Estado de resultado, conteniendo el pokemon modificado si corresponde.
 */
trait Actividad {
   def aplicar(pokemon : Pokemon) : Estado
   
   def ejecutar(pokemon : Pokemon) : Estado = {
     aplicar(pokemon)
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
    
    EstadoNormal(pokemon.cambiarExperiencia(experiencia))
  }
}

case class Nadar(minutos: Int) extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = {
    
    pokemon match {
      case _ if (Agua.mataA(pokemon.tipoPrincipal) || Agua.mataA(pokemon.tipoSecundario.getOrElse(Agua))) =>  EstadoKO(pokemon, "El pokemon pierde contra el agua")
      case _ => EstadoNormal(pokemon.cambiarExperiencia(200).cambiarEnergia(minutos).cambiarVelocidad(if (pokemon.tipoPrincipal == Agua) minutos % 60 else 0))
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
       case _ => EstadoNormal(pokemon.especie.condicionEvolucion.fold(pokemon)(_.evolucionar(pokemon, piedra)))
    }
  }
}

case object UsarPocion extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon.cambiarEnergia(50))
}

case object UsarAntidoto extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon)
}

case object UsarEther extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon)
}

case object ComerHierro extends Actividad {
  def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon.cambiarFuerza(5))
}

case object ComerCalcio extends Actividad {
   def aplicar(pokemon : Pokemon) : Estado = EstadoNormal(pokemon.cambiarVelocidad(5))
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
    val nuevoPokemon = pokemon.especie.condicionEvolucion.fold(pokemon)(_.evolucionar(pokemon))
    
    if (nuevoPokemon.esHembra)
      EstadoNormal(nuevoPokemon.cambiarPeso(-10))
    else
      EstadoNormal(nuevoPokemon.cambiarPeso(1))
  }
}