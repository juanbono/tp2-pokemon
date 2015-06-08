package utn.frba.pokemon

//si esta estructura funciona implementar todos los tipos
abstract class Tipo {
  def mataA : List[Tipo] 
}

case object Fuego extends Tipo {
  def mataA : List[Tipo] = List(Planta, Hielo, Bicho)
}

case object Agua extends Tipo {
  def mataA : List[Tipo] = List(Fuego, Tierra, Roca)
}

case object Planta extends Tipo {
  def mataA : List[Tipo] = List(Agua, Tierra, Roca)
}

case object Tierra extends Tipo {
  def mataA : List[Tipo] = List(Fuego, Electrico, Veneno, Roca)
}

case object Hielo extends Tipo {
  def mataA : List[Tipo] = List(Planta, Tierra, Volador, Dragon)
}

case object Roca extends Tipo {
  def mataA : List[Tipo] = List(Fuego, Hielo, Volador, Bicho)
}

case object Electrico extends Tipo {
  def mataA : List[Tipo] = List(Agua, Volador)
}

case object Psiquico extends Tipo {
  def mataA : List[Tipo] = List(Pelea, Veneno)
}

case object Pelea extends Tipo {
  def mataA : List[Tipo] = List(Normal, Hielo, Roca)
}

case object Fantasma extends Tipo {
  def mataA : List[Tipo] = List(Psiquico, Fantasma)
}

case object Volador extends Tipo {
  def mataA : List[Tipo] = List(Planta, Pelea, Bicho)
}

case object Bicho extends Tipo {
  def mataA : List[Tipo] = List(Planta, Psiquico)
}

case object Veneno extends Tipo {
  def mataA : List[Tipo] = List(Planta)
}

case object Dragon extends Tipo {
  def mataA : List[Tipo] = List(Dragon)
}

case object Normal extends Tipo {
  def mataA : List[Tipo] = List()
}
