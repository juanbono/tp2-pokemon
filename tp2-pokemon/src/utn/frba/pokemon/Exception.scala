package utn.frba.pokemon

// agregar las que hagan falta.
case class InvalidPokemonException(msg: String) extends Exception(msg)
case class UnknownAttackException(msg: String) extends Exception(msg)
case class KOException(msg: String) extends Exception(msg)
case class InvalidPokemonTypeException(msg: String) extends Exception(msg)
case class NoPAException(msg:String) extends Exception(msg)