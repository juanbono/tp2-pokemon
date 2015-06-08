package utn.frba.pokemon

case class ResultadoActividad(val pokemon : Option[Pokemon] = None, val puedeEjecutar : Boolean, val descripcion : String = "") {

}