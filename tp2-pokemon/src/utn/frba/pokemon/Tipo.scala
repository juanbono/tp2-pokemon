package utn.frba.pokemon

// Tipos que puede tener una especie de pokemon.
trait Tipo {
  def mataA(tipo : Tipo) : Boolean 
}

case object Fuego extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Planta, Hielo, Bicho).contains(tipo)
}

case object Agua extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Fuego, Tierra, Roca).contains(tipo)
}

case object Planta extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Agua, Tierra, Roca).contains(tipo)
}

case object Tierra extends Tipo{
  def mataA(tipo : Tipo) : Boolean = List(Fuego, Electrico, Veneno, Roca).contains(tipo)
}

case object Hielo extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Planta, Tierra, Volador, Dragon).contains(tipo)
}

case object Roca extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Fuego, Hielo, Volador, Bicho).contains(tipo)
}

case object Electrico extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Agua, Volador).contains(tipo)
}

case object Psiquico extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Pelea, Veneno).contains(tipo)
}

case object Pelea extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Normal, Hielo, Roca).contains(tipo)
}

case object Fantasma extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Psiquico, Fantasma).contains(tipo)
}

case object Volador extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Planta, Pelea, Bicho).contains(tipo)
}

case object Bicho extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Planta, Psiquico).contains(tipo)
}

case object Veneno extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Planta).contains(tipo)
}

case object Dragon extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List(Dragon).contains(tipo)
}

case object Normal extends Tipo {
  def mataA(tipo : Tipo) : Boolean = List().contains(tipo)
}
