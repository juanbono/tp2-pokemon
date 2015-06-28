package utn.frba.pokemon
import scala.util.{Try, Success, Failure}

object Simulador {
  def entrenar(p: Pokemon, rutina: Actividad*): Try[Pokemon] = rutina.foldLeft(Try(p)) { (estadoAnterior, actividadActual) =>  
    actividadActual match {
    }
  }
}
