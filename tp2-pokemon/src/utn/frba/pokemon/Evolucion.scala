package utn.frba.pokemon

abstract class PiedraEvolutiva {def tipo : Tipo}
case class PiedraEvolutivaComun(val tipo: Tipo) extends PiedraEvolutiva
case class PiedraLunar(val tipo: Tipo = Normal) extends PiedraEvolutiva

trait CondicionEvolucion {
  def evolucionar(pokemon: Pokemon) = pokemon
  def evolucionar(pokemon: Pokemon, piedra : PiedraEvolutiva) = pokemon
}

case class SubirNivelEvolucion(val nivel : Int) extends CondicionEvolucion {
  override def evolucionar(pokemon: Pokemon) = {
    if (pokemon.nivel >= nivel) {
      pokemon.evolucionar
    } else {
      pokemon
    }
  }
}

case object IntercambiarEvolucion extends CondicionEvolucion {
  override def evolucionar(pokemon: Pokemon) = pokemon.evolucionar
}

case object UsarPiedraEvolucion extends CondicionEvolucion {
  override def evolucionar(pokemon: Pokemon, piedra: PiedraEvolutiva) = {
    if (pokemon.especie.tipoPrincipal.equals(piedra.tipo)) {
      pokemon.evolucionar
    } else {
      pokemon
    }
  }
}

case class UsarPiedraLunarEvolucion() extends CondicionEvolucion {
  override def evolucionar(pokemon: Pokemon, piedra: PiedraEvolutiva) = {
    piedra match {
      case PiedraLunar(_) => pokemon.evolucionar
      case _ => pokemon
    }  
  }
}


