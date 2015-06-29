package utn.frba.pokemon
import scala.util.{ Try, Success, Failure }

object Simulador {

  type Rutina = (String, List[Actividad])
  def actividades(r: Rutina) = r._2
  def nombre(r: Rutina) = r._1
  
  type ResultadoRutina = (Try[Pokemon], Rutina)
  def pokemonEntrenado(rr: ResultadoRutina) = rr._1
  def rutinaRealizada(rr: ResultadoRutina) = rr._2

  def realizarActividad(p: Pokemon, actividad: Actividad): Try[Pokemon] = p.estado match {
    case None => Try(p).map(actividad)
    case Some(KO(msg))  => Failure(KOException(msg))
    case Some(Dormido(x)) => if (x == 3) Try(p) else Try(p.cambiarEstado(Some(Dormido(x-1))))
    case _ => Try(p).map(actividad)
  }

  def realizarRutina(p: Pokemon, rutina: Rutina): Try[Pokemon] = actividades(rutina).foldLeft(Try(p)) { (pokemonEntrenando, actividad) =>
    pokemonEntrenando match {
      case Success(_) => pokemonEntrenando.map(actividad)
      case Failure(_) => pokemonEntrenando
    }
  }

  def analizarRutinas(p: Pokemon, criterio: (Pokemon, Pokemon) => Boolean, rutinas: List[Rutina]): String = {
    val resultadosRutinas = rutinas.map { rutina => (realizarRutina(p, rutina), rutina) }.filter { resultado: ResultadoRutina => resultado._1.isSuccess }
    val mejorRutina = resultadosRutinas.sortWith((rr1, rr2) => criterio(pokemonEntrenado(rr1).get, pokemonEntrenado(rr2).get)).headOption

    mejorRutina match {
      case None     => "Ninguna rutina"
      case Some(rr) =>  nombre(rutinaRealizada(rr))
    }
  }
}
 