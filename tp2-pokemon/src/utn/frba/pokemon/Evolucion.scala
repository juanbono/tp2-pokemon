package utn.frba.pokemon

trait CondicionEvolucion {
  
}

class PiedraEvolutiva(tipo : Tipo)

object PiedraLunarEvolutiva //extends PiedraEvolutiva(tipo : Tipo) 

case class SubirNivelEvolucion(val nivel : Int) extends CondicionEvolucion

case class IntercambiarEvolucion() extends CondicionEvolucion

case class UsarPiedraEvolucion(val piedra : PiedraEvolutiva) extends CondicionEvolucion

case class UsarPiedraLunarEvolucion() extends CondicionEvolucion

