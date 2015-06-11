package utn.frba.pokemon

//sealed dice que ayuda al motor de inferencia a la hora del  pattern matching y los pongo como case objects para lo mismo 
// y porque no toman parametros ni nada.
sealed abstract class Genero {
  def intercambiar(pokemon: Pokemon): Pokemon
}

case object Masculino extends Genero {
  def intercambiar(pokemon: Pokemon): Pokemon = pokemon.subirPeso(1.0)
}

case object Femenino extends Genero {
  def intercambiar(pokemon: Pokemon): Pokemon = pokemon.bajarPeso(10.0)
}

case class Pokemon(
  val nivel: Int = 1,
  val experiencia: Int = 0,
  val genero: Genero = Masculino,
  val energia: Int = 0,
  val energiaMaxima: Int = 100,
  val peso: Double = 0,
  val fuerza: Int = 1,
  val velocidad: Int = 1,
  val especie: Especie,
  val ataques: List[Ataque] = List(new AtaqueDefault)) {

  def tipoPrincipal: Tipo = especie.tipoPrincipal
  def tipoSecundario: Option[Tipo] = especie.tipoSecundario
  def esMacho: Boolean = genero == Masculino
  def esHembra: Boolean = genero == Femenino
  def esTipoPrincipal(t: Tipo): Boolean = t == tipoPrincipal
  def esTipoSecundario(t: Tipo): Boolean = t == tipoSecundario.getOrElse(false)
  def algunTipoEs(t: Tipo): Boolean = (tipoPrincipal == t) || (tipoSecundario.getOrElse(false) == t)

  def subirVelocidad(dif: Int): Pokemon = copy(velocidad = this.velocidad + dif)
  def bajarVelocidad(dif: Int): Pokemon = copy(velocidad = this.velocidad - dif)

  def subirEnergia(dif: Int): Pokemon = copy(energia = this.energia + dif)
  def bajarEnergia(dif: Int): Pokemon = copy(energia = this.energia - dif)

  def subirExperiencia(dif: Int): Pokemon = copy(experiencia = this.experiencia + dif)
  def bajarExperiencia(dif: Int): Pokemon = copy(experiencia = this.experiencia - dif)

  def bajarPeso(dif: Double): Pokemon = copy(peso = peso - dif) // Falta controlar si es menor a 0.
  def subirPeso(dif: Double): Pokemon = copy(peso = peso + dif)

  def subirFuerza(dif: Int): Pokemon = copy(fuerza = this.fuerza + dif)
  def bajarFuerza(dif: Int): Pokemon = copy(fuerza = this.fuerza - dif)

  def experienciaNivel(nivel: Int): Int = {
    nivel match {
      case 1 => 0
      case x => 2 * experienciaNivel(x - 1) + especie.resistenciaEvolutiva
    }
  }

  def subirNivel: Pokemon = {
    if (experiencia >= experienciaNivel(nivel + 1))
      especie.condicionEvolucion.fold(copy(nivel = nivel + 1))(_.subirNivel(copy(nivel = nivel + 1)))
    else
      this
  }

  def usarPiedra(piedra : PiedraEvolutiva) : Pokemon = {
    especie.condicionEvolucion.fold(this)(_.usarPiedra(this, piedra))
  }
  
  def intercambiar : Pokemon = {
    especie.condicionEvolucion.fold(this)(_.intercambiar(this))
  }

  def evolucionar: Pokemon = {
    especie.evolucion.fold(this)(nuevaEspecie => copy(especie = nuevaEspecie))
  }
  
  def getPokemonValido : Option[Pokemon] = {
    
    Some(this)
  }
  
  
  //Validaciones
  def esNivelValido : Boolean = if (nivel < 1 || nivel > 100) false else true
  def esFuerzaValida : Boolean = if (fuerza < 1 || fuerza > 100) false else true
  def esGeneroValido : Boolean = if (genero != Masculino && genero != Femenino) false else true
  def esVelocidadValida : Boolean = if (velocidad < 1 || velocidad > 100) false else true
  def esPesoValido : Boolean = if (peso < 0 || peso > 100) false else true
  
  def esPokemonValido() : Boolean = esNivelValido && esFuerzaValida && esGeneroValido && esVelocidadValida && esPesoValido
}


