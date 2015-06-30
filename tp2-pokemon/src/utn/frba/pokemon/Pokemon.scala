package utn.frba.pokemon
import utn.frba.pokemon._

//sealed dice que ayuda al motor de inferencia a la hora del  pattern matching y los pongo como case objects para lo mismo 
// y porque no toman parametros ni nada.
sealed abstract class Genero
case object Masculino extends Genero
case object Femenino extends Genero

case class Pokemon(
  val experiencia: Int = 0,
  val genero: Genero = Masculino,
  val energia: Int = 0,
  val pesoExtra: Double = 0,
  val fuerzaExtra: Int = 0,
  val velocidadExtra: Int = 0,
  val especie: Especie,
  val estado: Option[Estado] = None,
  val ataques: List[Ataque]) {

  def tipoPrincipal: Tipo = especie.tipoPrincipal
  def tipoSecundario: Option[Tipo] = especie.tipoSecundario
  def esMacho: Boolean = genero == Masculino
  def esHembra: Boolean = genero == Femenino
  def esTipoPrincipal(t: Tipo): Boolean = t == tipoPrincipal
  def esTipoSecundario(t: Tipo): Boolean = t == tipoSecundario.getOrElse(false)
  def algunTipoEs(t: Tipo): Boolean = esTipoPrincipal(t) || esTipoSecundario(t)
  def tieneEstado(t: Estado): Boolean = this.estado match {
    case None                => false
    case Some(v) if (v == t) => false
    case _                   => false
  }
  def cambiarEstado(e: Option[Estado]): Pokemon = e match {
    case None    => copy(estado = None)
    case Some(s) => copy(estado = Some(s))
  }
  def estaKO: Boolean = this.estado match {
    case Some(KO(_)) => true
    case _ => false
  }

  def subirExperiencia(dif: Int): Pokemon = {
    if (experiencia + dif < 0) {
      throw InvalidPokemonException("Pokemon no puede disminuir la experiencia")
    }

    val nuevoPokemon = copy(experiencia = experiencia + dif)

    especie.condicionEvolucion.fold(nuevoPokemon)(_.subirNivel(nuevoPokemon))
  }

  def velocidad = especie.velocidadInc * this.nivel + velocidadExtra

  def cambiarVelocidad(dif: Int): Pokemon = {
    if (velocidad + dif < 1) {
      throw InvalidPokemonException("Pokemon no puede disminuir la velocidad")
    }

    val cambioVelocidad = if (velocidad + dif > 100) {
      100
    } else {
      this.velocidadExtra + dif
    }

    copy(velocidadExtra = cambioVelocidad)
  }

  def energiaMaxima = especie.energiaMaximaInc * this.nivel

  def cambiarEnergia(dif: Int): Pokemon = {
    if (energia + dif < 0) {
      throw InvalidPokemonException("Pokemon no puede disminuir la energia")
    }

    val cambioEnergia = if (energia + dif > energiaMaxima) {
      energiaMaxima
    } else {
      this.energia + dif
    }

    copy(energia = cambioEnergia)
  }

  def peso: Double = especie.pesoInc * this.nivel + pesoExtra

  def cambiarPeso(dif: Double): Pokemon = {
    if (peso + dif > especie.pesoMaximo) {
      throw InvalidPokemonException("Pokemon no puede aumentar el peso")
    } else if (peso + dif < 0) {
      throw InvalidPokemonException("Pokemon no puede disminuir el peso")
    }

    copy(pesoExtra = this.pesoExtra + dif)
  }

  def fuerza = especie.fuerzaInc * this.nivel + fuerzaExtra

  def cambiarFuerza(dif: Int): Pokemon = {
    if (fuerza + dif < 1) {
      throw InvalidPokemonException("Pokemon no puede disminuir la fuerza")
    }

    val cambioFuerza = if (fuerza + dif > 100) {
      100
    } else {
      this.fuerzaExtra + dif
    }

    copy(fuerzaExtra = cambioFuerza)
  }

  // Debe haber una forma de hacerlo mas eficiente.
  def nivel: Int = {
    for (i <- 1 to 100) {
      if (this.experienciaNivel(i + 1) > experiencia) {
        return i
      }
    }

    return 100
  }

  def experienciaNivel(nivel: Int): Int = {
    nivel match {
      case 1 => 0
      case x => 2 * experienciaNivel(x - 1) + especie.resistenciaEvolutiva
    }
  }

  def evolucionar: Pokemon = especie.evolucion.fold(this)(nuevaEspecie => copy(especie = nuevaEspecie))

  def getPokemonValido: Option[Pokemon] = Some(this)
}


