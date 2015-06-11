package utn.frba.pokemon

object Simulador {
  def entrenar(p: Pokemon, rutina: Actividad*): Estado = rutina.foldLeft(EstadoNormal(p): Estado) { (estadoAnterior, actividadActual) =>
    
    actividadActual match {
      // La logica de los estados esta en cada actividad. Permite especificar mejor los mensajes cuando no se puedo realizar una actividad.
      // Los cases de aca abajo son aquellos que tienen un comportamiento que depende del estado anterior. El resto solo consiste en ejecutar la actividad. 
      
      case LevantarPesas(kilos) => estadoAnterior match {
        case EstadoParalizado(_) => estadoAnterior.flatMap { pokemon => EstadoKO(pokemon, "Pokemon paralizado intento levantar pesas") }
        case _ => estadoAnterior.flatMap { pokemon => actividadActual.ejecutar(pokemon) }
      }
      
      case UsarAntidoto => estadoAnterior match {
        case EstadoEnvenenado(_) => estadoAnterior.flatMap { pokemon => EstadoNormal(pokemon) }
        case _ => estadoAnterior.map { pokemon => pokemon }
      }
      
      case Descansar => estadoAnterior match {
        case EstadoNormal(_) =>  estadoAnterior.flatMap { pokemon => actividadActual.ejecutar(pokemon)}
        case _ => estadoAnterior.map { pokemon => pokemon }
      }
      
      case otraActividad =>
        estadoAnterior.flatMap { pokemon => actividadActual.ejecutar(pokemon)}
        
    }
  }
}
