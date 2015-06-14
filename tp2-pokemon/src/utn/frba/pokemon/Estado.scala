package utn.frba.pokemon

 //**************************** Estados ***********************************
abstract class Estado 
  case object Envenenado extends Estado
  case class KO(msg: String) extends Estado
  case object Paralizado extends Estado
  case class Dormido(turnos: Int) extends Estado
  
  
  
/*
trait Estado {
  def map(f: (Pokemon => Pokemon)): Estado
  def filter(f: (Pokemon => Boolean)): Estado
  def flatMap(f: (Pokemon => Estado)): Estado
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T
}

case class EstadoNormal extends Estado {
  def map(f: (Pokemon => Pokemon)): Estado = EstadoNormal(f(pokemon))
  def filter(f: (Pokemon => Boolean)): Estado = if (f(pokemon)) this else EstadoActividadNoEjecutada(pokemon, "%s no puede hacer la actividad %s".format(pokemon, f))
  def flatMap(f: (Pokemon => Estado)): Estado = f(pokemon)
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = f(pokemon)
}

case class EstadoEnvenenado(val pokemon: Pokemon) extends Estado { // hay que agregarle algun efecto raro, por ahora es como EstadoNormal 
  def map(f: (Pokemon => Pokemon)): Estado = EstadoEnvenenado(f(pokemon))
  def filter(f: (Pokemon => Boolean)): Estado = if (f(pokemon)) this else EstadoActividadNoEjecutada(pokemon, "%s no puede hacer la actividad %s".format(pokemon, f))
  def flatMap(f: (Pokemon => Estado)): Estado = f(pokemon)
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = f(pokemon)
}

case class EstadoParalizado(val pokemon: Pokemon) extends Estado {
  def map(f: (Pokemon => Pokemon)): Estado = EstadoParalizado(f(pokemon))
  def filter(f: (Pokemon => Boolean)): Estado = if (f(pokemon)) this else EstadoActividadNoEjecutada(pokemon, "%s no puede hacer la actividad %s".format(pokemon, f))
  def flatMap(f: (Pokemon => Estado)): Estado = f(pokemon)
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = f(pokemon)
}

case class EstadoKO(val pokemon: Pokemon,val descripcion: String) extends Estado {
  def map(f: (Pokemon => Pokemon)): Estado = this
  def filter(f: (Pokemon => Boolean)): Estado = this
  def flatMap(f: (Pokemon => Estado)): Estado = this
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = e(pokemon)
}

case class EstadoDormido(val pokemon: Pokemon, turnosDormido: Int = 1) extends Estado {
  def map(f: (Pokemon => Pokemon)): Estado = if (turnosDormido == 3) EstadoNormal(pokemon) else EstadoDormido(pokemon, turnosDormido + 1)
  def filter(f: (Pokemon => Boolean)): Estado = this
  def flatMap(f: (Pokemon => Estado)): Estado = this
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = e(pokemon)
}

//Nuevo estado EstadoActividadNoEjecutada, para cuando un pokemon no puede ejecutar la actividad. No me parecia que el KO sirviera, porque puede no 
//realizar una actividad pero si la siguiente y si esta KO no la haria.
case class EstadoActividadNoEjecutada(val pokemon: Pokemon, val descripcion: String) extends Estado {
  def map(f: (Pokemon => Pokemon)): Estado = EstadoNormal(f(pokemon))
  def filter(f: (Pokemon => Boolean)): Estado = if (f(pokemon)) this else EstadoKO(pokemon, "%s no puede hacer la actividad %s".format(pokemon, f))
  def flatMap(f: (Pokemon => Estado)): Estado = f(pokemon)
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = f(pokemon)
}
*/