package utn.frba.pokemon

//ver si hay alguna otra forma para genero
object  Genero {
  val masculino : Short = 0
  val femenino : Short = 1
}

case class Pokemon (
    val nivel : Int = 1, 
    val experiencia : Int = 0, 
    val genero : Int = Genero.masculino, 
    val energia : Int = 0, 
    val energiaMaxima : Int = 100, 
    val peso : Float = 0, 
    val fuerza : Int = 0,
    val especie : Especie,
    val estado : Estado = EstadoNormal,
    val ataques : Option[List[Ataque]])
    {
   

   def experienciaNivel(nivel : Int) : Int = {
     nivel match {
      case 1 => 0
      case x => 2 * experienciaNivel(x - 1) + especie.resistenciaEvolutiva
     }
   }
   
   def subirNivel : Pokemon = {
     if (experiencia >= experienciaNivel(nivel + 1))
       copy(nivel = nivel + 1)
     else
       this
   }
   
}


