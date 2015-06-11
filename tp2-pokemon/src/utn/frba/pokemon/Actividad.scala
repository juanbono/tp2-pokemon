package utn.frba.pokemon

trait Actividad {
   def ejecutar(pokemon : Pokemon) : Pokemon
}

case class RealizarUnAtaque(ataque: Ataque) extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = {
    ataque.ejecutar(pokemon).subirNivel
  }
}

case class LevantarPesas(kilos: Int) extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = {
    var experiencia = kilos
    if (pokemon.algunTipoEs(Pelea))
      experiencia = experiencia * 2
    
    pokemon.copy(experiencia = experiencia).subirNivel
  }
}
case class Nadar(minutos: Int) extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = {
    pokemon.copy(energia = pokemon.energia - minutos, experiencia = pokemon.experiencia + 200, velocidad = if (pokemon.tipoPrincipal == Agua) pokemon.velocidad + minutos % 60 else pokemon.velocidad).subirNivel
  }
} 

case class AprenderAtaque(ataque: Ataque) extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = { 
    pokemon.copy(ataques = ataque.copy(puntosDeAtaque = ataque.maximoInicialPA) :: pokemon.ataques).subirNivel
  }
}
case class UsarPiedra(piedra: PiedraEvolutiva) extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = { 
    pokemon.usarPiedra(piedra).subirNivel
  }
}

case object UsarPocion extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = pokemon.subirEnergia(50).subirNivel
}

case object UsarAntidoto extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = { pokemon }
}

case object UsarEther extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = { pokemon }
}

case object ComerHierro extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon =  pokemon.subirFuerza(5).subirNivel
}

case object ComerCalcio extends Actividad {
   def ejecutar(pokemon : Pokemon) : Pokemon = pokemon.subirVelocidad(5).subirNivel
}

case object ComerZinc extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = {
   val ataquesModificados = pokemon.ataques.map (ataque => ataque.copy(maximoInicialPA = ataque.maximoInicialPA + 2) )
   pokemon.copy(ataques = ataquesModificados).subirNivel
  }
}

case object Descansar extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = {
   val ataquesModificados = pokemon.ataques.map (ataque => ataque.copy(puntosDeAtaque = ataque.maximoInicialPA) )
   pokemon.copy(ataques = ataquesModificados).subirNivel
  }
}

case object FingirIntercambio extends Actividad {
  def ejecutar(pokemon : Pokemon) : Pokemon = {
    if (pokemon.especie.condicionEvolucion == IntercambiarEvolucion)
      return pokemon.intercambiar.subirNivel
    
    if (pokemon.esHembra)
      pokemon.subirPeso(-10).subirNivel
    else
      pokemon.subirPeso(1).subirNivel
    
  }
}