package utn.frba.pokemon

// Los ataques de ejemplo como Reposar, Enfocarse y Endurecerse deberian ser clases creadas en nuestro modelo, 
// o instancias de Ataque donde se le pasa el comportamiento que hace el ataque + el efecto??
trait Ataque {
  def tipo : TipoPokemon
  def puntosDeAtaque : Int
  def maximoInicialPA : Int
  
  def ejecutar(pokemon : Pokemon) : Pokemon 
}

case class MordidaAtaque(val puntosDeAtaque : Int, val maximoInicialPA : Int = 30) extends Ataque {
  
  def tipo : TipoPokemon = Fuego
  
  def ejecutar(pokemon : Pokemon) : Pokemon = {
    pokemon.copy(energia = pokemon.energiaMaxima, estado = EstadoDormido)
  }
}

case class ReporsarAtaque(val puntosDeAtaque : Int, val maximoInicialPA : Int) extends Ataque {
  def tipo : TipoPokemon = Fuego
   
  def ejecutar(pokemon : Pokemon) : Pokemon = {
    pokemon.copy(energia = pokemon.energiaMaxima, estado = EstadoDormido)
  }
}

case class EnfocarseAtaque(val puntosDeAtaque : Int, val maximoInicialPA : Int) extends Ataque {
  def tipo : TipoPokemon = Fuego
   
  def ejecutar(pokemon : Pokemon) : Pokemon = {
    pokemon.copy(velocidad = pokemon.velocidad + 1)
  }
}

case class EndurecerseAtaque(val puntosDeAtaque : Int, val maximoInicialPA : Int) extends Ataque {
  def tipo : TipoPokemon = Fuego
   
  def ejecutar(pokemon : Pokemon) : Pokemon = {
    pokemon.copy(energia = pokemon.energia + 5, estado = EstadoParalizado)
  }
}