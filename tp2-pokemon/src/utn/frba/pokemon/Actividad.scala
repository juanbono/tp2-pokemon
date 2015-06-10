package utn.frba.pokemon

abstract class Actividad

case class RealizarUnAtaque(ataque: Ataque) extends Actividad
case class LevantarPesas(kilos: Int) extends Actividad
case class Nadar(minutos: Int) extends Actividad
case class AprenderAtaque(ataque: Ataque) extends Actividad
//case class usarPiedra(piedra: Lunar) extends Actividad

case object UsarPocion extends Actividad
case object UsarAntidoto extends Actividad
case object UsarEther extends Actividad
case object ComerHierro extends Actividad
case object ComerCalcio extends Actividad
case object ComerZinc extends Actividad
case object Descansar extends Actividad
case object FingirIntercambio extends Actividad