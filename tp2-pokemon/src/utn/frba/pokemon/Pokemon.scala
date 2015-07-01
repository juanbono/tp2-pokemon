package utn.frba.pokemon
import scala.util.{Try, Success, Failure}

//sealed dice que ayuda al motor de inferencia a la hora del  pattern matching y los pongo como case objects para lo mismo 
// y porque no toman parametros ni nada.
sealed abstract class Genero
case object Masculino extends Genero
case object Femenino extends Genero

object Pokemon {
  // Builder de Pokemon, instancia un pokemon y lo evoluciona de acuerdo a su experiencia.
  def make(
          experiencia: Long = 0,
          especie: Especie,
          ataques: List[Ataque] = List(AtaqueDefault),
          genero: Genero = Masculino,
          energia: Int = 0,
          estado: Estado = EstadoNormal,
          energiaMaximaExtra: Int = 0,
          pesoExtra: Double = 0 ,
          fuerzaExtra: Int = 0,
          deltaVelocidad: Int = 0
        ) : Try[Pokemon] = {

     Try(Pokemon(experiencia, especie, ataques, genero, energia, estado, energiaMaximaExtra, pesoExtra, fuerzaExtra, deltaVelocidad, List()).actualizarEvoluciones())
    }
  }
  
case class Pokemon private (
  val experiencia: Long,
  val especie: Especie,
  val ataques: List[Ataque],
  val genero: Genero,
  val energia: Int,
  val estado: Estado,
   energiaMaximaExtra: Int,
   pesoExtra: Double,
   fuerzaExtra: Int,
   velocidadExtra: Int,
   evoluciones : List[(Int, Especie)] //guarda la historia de las evoluciones
  ) {

  // Inicializacion atributos calculados
  val nivel = calcularNivel
  val energiaMaxima =  especie.energiaMaximaInc * nivel + energiaMaximaExtra
  val peso =  especie.pesoInc * nivel + pesoExtra
  val fuerza = especie.fuerzaInc * nivel + fuerzaExtra
  val velocidad = especie.velocidadInc * nivel + velocidadExtra
  
  require (fuerza >= 1 && fuerza <= 100, "La fuerza debe ser un valor entre 1 y 100. Actual: %d".format(fuerza))
  require (peso >= 0 && peso <= especie.pesoMaximo, "El peso debe ser un valor entre 0 y %f. Actual: %f".format(especie.pesoMaximo, peso))
  require (velocidad >= 1 && velocidad <= 100, "La velocidad debe ser un valor entre 1 y 100. Actual: %d".format(velocidad))      
  require (energia >= 0 && energia <= energiaMaxima, "La energia debe ser un valor entre 0 y %d. Actual: %d".format(energiaMaxima, energia))
  
  
  //Calcula el nivel en base a la experiencia
  private def calcularNivel : Int = {
    Range(1, 101).find { nivel => experienciaNivel(nivel + 1) > this.experiencia }.getOrElse(1)
  }
  
  //Devuelve experiencia necesaria para X nivel teniendo en cuenta las resistencias evolutivas pasadas.
  def experienciaNivel(nivel: Int): Long = {
    nivel match {
      case 1 => 0
      case x => 2 * experienciaNivel(x - 1) + evoluciones.find { tupla => x <= tupla._1 }.fold(especie)(tupla => tupla._2).resistenciaEvolutiva
    }
  }
  
  private def actualizarEvoluciones() : Pokemon = {
    especie.condicionEvolucion.fold(this)(_.evolucionar(this))
  }
  
  def evolucionar: Pokemon = {
    especie.evolucion.fold(this)(nuevaEspecie => 
      if (especie != nuevaEspecie)
        copy(especie = nuevaEspecie, evoluciones = evoluciones :+ (this.nivel, this.especie))
      else
        this
      )
  }
  
  def tipoPrincipal: Tipo = especie.tipoPrincipal
  def tipoSecundario: Option[Tipo] = especie.tipoSecundario
  def esMacho: Boolean = genero == Masculino
  def esHembra: Boolean = genero == Femenino
  def esTipoPrincipal(t: Tipo): Boolean = t == tipoPrincipal
  def esTipoSecundario(t: Tipo): Boolean = t == tipoSecundario.getOrElse(false)
  def algunTipoEs(t: Tipo): Boolean = (tipoPrincipal == t) || (tipoSecundario.getOrElse(false) == t)

  def cambiarVelocidad(dif: Int): Pokemon = copy(velocidadExtra = this.velocidadExtra + dif)
  def cambiarEnergia(dif: Int): Pokemon = copy(energia = this.energia + dif)
  def cambiarPeso(dif: Double): Pokemon = copy(pesoExtra = peso + dif)
  def cambiarFuerza(dif: Int): Pokemon = copy(fuerzaExtra = this.fuerzaExtra + dif)
  def cambiarExperiencia(dif: Long): Pokemon = copy(experiencia = experiencia + dif).actualizarEvoluciones()
  def cambiarEstado(estado : Estado): Pokemon = copy(estado = estado)
  
  def puedeHacerActividad : Boolean = false

}


