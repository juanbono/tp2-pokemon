package utn.frba.pokemon

case class Tipo()

object Lunar extends Tipo

// Tipos que puede tener una especie de pokemon.
abstract class TipoPokemon extends Tipo {
  def mataA : List[TipoPokemon] 
}

object Fuego extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Planta, Hielo, Bicho)
}

object Agua extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Fuego, Tierra, Roca)
}

object Planta extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Agua, Tierra, Roca)
}

object Tierra extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Fuego, Electrico, Veneno, Roca)
}

object Hielo extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Planta, Tierra, Volador, Dragon)
}

object Roca extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Fuego, Hielo, Volador, Bicho)
}

object Electrico extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Agua, Volador)
}

object Psiquico extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Pelea, Veneno)
}

object Pelea extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Normal, Hielo, Roca)
}

object Fantasma extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Psiquico, Fantasma)
}

object Volador extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Planta, Pelea, Bicho)
}

object Bicho extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Planta, Psiquico)
}

object Veneno extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Planta)
}

object Dragon extends TipoPokemon {
  def mataA : List[TipoPokemon] = List(Dragon)
}

object Normal extends TipoPokemon {
  def mataA : List[TipoPokemon] = List()
}
