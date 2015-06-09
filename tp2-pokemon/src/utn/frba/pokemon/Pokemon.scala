package utn.frba.pokemon

//sealed dice que ayuda al motor de inferencia a la hora del  pattern matching y los pongo como case objects para lo mismo 
// y porque no toman parametros ni nada.
sealed abstract class Genero {
  def intercambiar(pokemon : Pokemon) : Pokemon
} 

case object Masculino extends Genero {
  def intercambiar(pokemon : Pokemon) : Pokemon = pokemon.subirPeso(1.0)
}

case object Femenino extends Genero {
  def intercambiar(pokemon : Pokemon) : Pokemon = pokemon.bajarPeso(10.0)
}

case class Pokemon (
    val nivel : Int = 1, 
    val experiencia : Int = 0, 
    val genero : Genero = Masculino, 
    val energia : Int = 0, 
    val energiaMaxima : Int = 100, 
    val peso : Double = 0, 
    val fuerza : Int = 1,
    val velocidad : Int = 1,
    val especie : Especie,
    val estado : Estado = EstadoNormal,
    val ataques : Option[List[Ataque]] = None) {   

    require(nivel >= 1 && nivel <= 100, "Nivel es un número de 1 a 100")
    require(genero == Masculino || genero == Femenino, "Genero puede ser Masculino o Femenino")
    require(fuerza >= 1 && fuerza <= 100, "Fuerza es un número de 1 a 100")
    require(velocidad >= 1 && velocidad <= 100, "Velocidad es un número de 1 a 100")

  def subirPeso(dif : Double) : Pokemon = copy(peso = peso + dif)
  
  // Falta controlar si es menor a 0.
  def bajarPeso(dif : Double) : Pokemon = copy(peso = peso - dif)
  
   def experienciaNivel(nivel : Int) : Int = {
     nivel match {
      case 1 => 0
      case x => 2 * experienciaNivel(x - 1) + especie.resistenciaEvolutiva
     }
   }
   
  def subirNivel : Pokemon = {
    if (experiencia >= experienciaNivel(nivel + 1))
      especie.condicionEvolucion.fold(this)(_.subirNivel(copy(nivel = nivel + 1)))
    else
      this
  }

  def usarPiedra(piedra : PiedraEvolutiva) : Pokemon = {
    especie.condicionEvolucion.fold(this)(_.usarPiedra(this, piedra))
  }
  
  def intercambiar : Pokemon = {
    especie.condicionEvolucion.fold(this)(_.intercambiar(this))
  }
  
  def evolucionar : Pokemon = {
    especie.evolucion.fold(this)(nuevaEspecie => copy(especie = nuevaEspecie))
  }
}


