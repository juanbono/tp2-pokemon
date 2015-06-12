package utn.frba.pokemon

//sealed dice que ayuda al motor de inferencia a la hora del  pattern matching y los pongo como case objects para lo mismo 
// y porque no toman parametros ni nada.
sealed abstract class Genero
case object Masculino extends Genero
case object Femenino extends Genero

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

  def subirEnergia(dif: Int): Pokemon = copy(energia = this.energia + dif)
  def bajarEnergia(dif: Int): Pokemon = copy(energia = this.energia - dif)

  def subirExperiencia(dif: Int): Pokemon = copy(experiencia = experiencia + dif).subirNivel

  def bajarPeso(dif: Double): Pokemon = copy(peso = peso - dif)
  def subirPeso(dif: Double): Pokemon = copy(peso = peso + dif)

  def subirFuerza(dif: Int): Pokemon = copy(fuerza = this.fuerza + dif)

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

  def evolucionar: Pokemon = {
    especie.evolucion.fold(this)(nuevaEspecie => copy(especie = nuevaEspecie))
  }
  
  def getPokemonValido : Option[Pokemon] = {
    Some(this)
  }
  
  
  //Validaciones
  def esNivelValido : Boolean = (nivel >= 1 && nivel <= 100) 
  def esFuerzaValida : Boolean = (fuerza >= 1 && fuerza <= 100)
  def esGeneroValido : Boolean = (genero == Masculino || genero == Femenino)
  def esVelocidadValida : Boolean = (velocidad >= 1 && velocidad <= 100) 
  def esPesoValido : Boolean = (peso >= 0 && peso <= 100) 
  
  def esPokemonValido : Boolean = esNivelValido && esFuerzaValida && esGeneroValido && esVelocidadValida && esPesoValido
}


