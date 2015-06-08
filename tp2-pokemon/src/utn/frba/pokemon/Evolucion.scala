package utn.frba.pokemon

case class PiedraEvolutiva(val tipo : Tipo)

trait CondicionEvolucion {
  def intercambiar(pokemon: Pokemon) = {
    pokemon
  }
  
  def subirNivel(pokemon: Pokemon) = {
    pokemon
  }
  
  def usarPiedra(pokemon: Pokemon, piedra: PiedraEvolutiva) = {
    pokemon
  }
}

case class SubirNivelEvolucion(val nivel : Int) extends CondicionEvolucion {
  override def subirNivel(pokemon: Pokemon) = {
    if (pokemon.nivel >= nivel) {
      pokemon.evolucionar;
    } else {
      pokemon
    }
  }
}

case class IntercambiarEvolucion() extends CondicionEvolucion {
  override def intercambiar(pokemon: Pokemon) = {
    pokemon.evolucionar;
  }
}

case class UsarPiedraEvolucion(val piedra : PiedraEvolutiva) extends CondicionEvolucion {
  override def usarPiedra(pokemon: Pokemon, piedra: PiedraEvolutiva) = {
    if (pokemon.especie.tipoPrincipal.equals(piedra.tipo)) {
      pokemon.evolucionar
    } else {
      pokemon
    }
  }
}

case class UsarPiedraLunarEvolucion() extends CondicionEvolucion {
  override def usarPiedra(pokemon: Pokemon, piedra: PiedraEvolutiva) = {
    if (piedra.tipo.equals(Lunar)) {
      pokemon.evolucionar
    } else {
      pokemon
    }
  }
}

