package utn.frba.pokemon

abstract class PiedraEvolutiva {def tipo : Tipo}
case class PiedraEvolutivaComun(val tipo: Tipo) extends PiedraEvolutiva
case class PiedraLunar(val tipo: Tipo = Normal) extends PiedraEvolutiva

trait CondicionEvolucion {
  def intercambiar(pokemon: Pokemon) = pokemon
  def subirNivel(pokemon: Pokemon) = pokemon
  def usarPiedra(pokemon: Pokemon, piedra : PiedraEvolutiva) = pokemon 
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
  override def intercambiar(pokemon: Pokemon) = pokemon.evolucionar
}

case class UsarPiedraEvolucion() extends CondicionEvolucion {
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
    piedra match {
      case PiedraLunar(_) => pokemon.evolucionar
      case _ => pokemon
    }  
  }
}


