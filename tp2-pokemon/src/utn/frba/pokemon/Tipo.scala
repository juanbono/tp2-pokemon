package utn.frba.pokemon

case object Lunar {
  def mataA(tipo : TipoPokemon) : Boolean = false
}

// Tipos que puede tener una especie de pokemon.
abstract class TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean 
}

case object Fuego extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Planta, Hielo, Bicho).contains(tipo)
}

case object Agua extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Fuego, Tierra, Roca).contains(tipo)
}

case object Planta extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Agua, Tierra, Roca).contains(tipo)
}

case object Tierra extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Fuego, Electrico, Veneno, Roca).contains(tipo)
}

case object Hielo extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Planta, Tierra, Volador, Dragon).contains(tipo)
}

case object Roca extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Fuego, Hielo, Volador, Bicho).contains(tipo)
}

case object Electrico extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Agua, Volador).contains(tipo)
}

case object Psiquico extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Pelea, Veneno).contains(tipo)
}

case object Pelea extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Normal, Hielo, Roca).contains(tipo)
}

case object Fantasma extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Psiquico, Fantasma).contains(tipo)
}

case object Volador extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Planta, Pelea, Bicho).contains(tipo)
}

case object Bicho extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Planta, Psiquico).contains(tipo)
}

case object Veneno extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Planta).contains(tipo)
}

case object Dragon extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List(Dragon).contains(tipo)
}

case object Normal extends TipoPokemon {
  def mataA(tipo : TipoPokemon) : Boolean = List().contains(tipo)
}
