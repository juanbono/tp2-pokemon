package utn.frba.pokemon

class Rutina(nombre : String, actividades : Actividad*) {
  def ejecutar(pokemon : Pokemon) : Estado = {
    Simulador.entrenar(pokemon, actividades: _*)
  }
}


class AnalizadorRutinas(criterio : (Rutina => Int), rutinas : Rutina*) {
  def mejorRutina(pokemon : Pokemon) : String = {
    val resultadosRutinas = rutinas.map { rutina => rutina.ejecutar(pokemon) }
    
    
    "nombre mejor rutina"
  }
}
