package utn.frba.pokemon

abstract class Estado {
  def pokemon: Pokemon
  def map(f: (Pokemon => Pokemon)): Estado
  def filter(f: (Pokemon => Boolean)): Estado
  def flatMap(f: (Pokemon => Estado)): Estado
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T
}

case class EstadoNormal(val pokemon: Pokemon) extends Estado {
  def map(f: (Pokemon => Pokemon)): Estado = EstadoNormal(f(pokemon))
  def filter(f: (Pokemon => Boolean)): Estado = if (f(pokemon)) this else KO(pokemon, "p no puede hacer la actividad f") // ver si se puede pasar a string p y f
  def flatMap(f: (Pokemon => Estado)): Estado = f(pokemon)
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = f(pokemon)
}

case class Envenenado(val pokemon: Pokemon) extends Estado { // hay que agregarle algun efecto raro, por ahora es como EstadoNormal 
  def map(f: (Pokemon => Pokemon)): Estado = Envenenado(f(pokemon))
  def filter(f: (Pokemon => Boolean)): Estado = if (f(pokemon)) this else KO(pokemon, "p no puede hacer la actividad f")
  def flatMap(f: (Pokemon => Estado)): Estado = f(pokemon)
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = f(pokemon)
}

case class Paralizado(val pokemon: Pokemon) extends Estado {
  def map(f: (Pokemon => Pokemon)): Estado = if (f.isInstanceOf[LevantarPesas]) KO(pokemon, "p ha quedado KO al levantar pesas paralizado") else Paralizado(f(pokemon))
  def filter(f: (Pokemon => Boolean)): Estado = if (f(pokemon)) this else KO(pokemon, "p no puede hacer la actividad f")
  def flatMap(f: (Pokemon => Estado)): Estado = f(pokemon)
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = f(pokemon)
}

case class KO(val pokemon: Pokemon,val descripcion: String) extends Estado {
  def map(f: (Pokemon => Pokemon)): Estado = this
  def filter(f: (Pokemon => Boolean)): Estado = this
  def flatMap(f: (Pokemon => Estado)): Estado = this
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = e(pokemon)
}

case class Dormido(val pokemon: Pokemon, turnosDormido: Int = 1) extends Estado {
  def map(f: (Pokemon => Pokemon)): Estado = if (turnosDormido == 3) EstadoNormal(pokemon) else Dormido(pokemon, turnosDormido + 1)
  def filter(f: (Pokemon => Boolean)): Estado = this
  def flatMap(f: (Pokemon => Estado)): Estado = this
  def fold[T](e: (Pokemon => T))(f: (Pokemon => T)): T = e(pokemon)
}
