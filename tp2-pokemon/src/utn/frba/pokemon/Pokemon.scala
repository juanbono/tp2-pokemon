package utn.frba.pokemon

//sealed dice que ayuda al motor de inferencia a la hora del  pattern matching y los pongo como case objects para lo mismo 
// y porque no toman parametros ni nada.
sealed abstract class Genero  
case object Masculino extends Genero
case object Femenino extends Genero

case class Pokemon (
    val nivel : Int = 1, 
    val experiencia : Int = 0, 
    val genero : Genero = Masculino, 
    val energia : Int = 0, 
    val energiaMaxima : Int = 100, 
    val peso : Float = 0, 
    val fuerza : Int = 0,
    val velocidad : Int = 0,
    val especie : Especie,
    val estado : Estado = EstadoNormal,
    val ataques : Option[List[Ataque]] = None) {   

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
   
  def evolucionar : Pokemon = {
    especie.evolucion.fold(this)(nuevaEspecie => copy(especie = nuevaEspecie))
  }
}


