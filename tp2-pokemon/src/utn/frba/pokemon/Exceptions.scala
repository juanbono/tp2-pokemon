package utn.frba.pokemon

case class InvalidPokemonException(msg: String) extends Exception(msg)
case class UnknownAttackException(msg: String) extends Exception(msg)
case class KOException(msg: String) extends Exception(msg)
case class InvalidPokemonTypeException(msg: String) extends Exception(msg)
case class PuntosDeAtaqueInsuficientesExceltion(msg: String) extends Exception(msg)