package utn.frba.pokemon

class Rutina(val nombre: String, val actividades: Actividad*) {
  def ejecutar(pokemon: Pokemon): ResultadoRutina = {
    ResultadoRutina(rutina = this, estado = Simulador.entrenar(pokemon, actividades: _*))
  }
}

case class ResultadoRutina(val rutina: Rutina, val estado: Estado) {
  val pokemon = estado.pokemon

  def termino: Boolean = {
    // mejorar.
    // devuelve si la rutina termino bien. Ver que se hace si termina en estado DOrmido, Envenenado, etc...
    // ver cual es la mejor forma de saber si pudo termina la rutina bien?
    estado match {
      case EstadoNormal(_) => true
      case _               => false
    }
  }
}

class AnalizadorRutinas(criterio: (ResultadoRutina, ResultadoRutina) => Boolean, rutinas: Rutina*) {
  def mejorRutina(pokemon: Pokemon): String = {
    val resultadosRutinas = rutinas.map { rutina => rutina.ejecutar(pokemon) }.filter { resultado => resultado.termino }
    val mejorRutina = resultadosRutinas.sortWith(criterio).headOption

    mejorRutina match {
      case None    => "Ninguna rutina"
      case Some(a) => a.rutina.nombre
    }
  }
}
