package utn.frba.pokemon

//si esta estructura funciona implementar todos los tipos
trait Tipo {
  def mataA : List[Tipo] 
}

object Fuego extends Tipo {
  def mataA : List[Tipo] = List(Planta)
}

object Planta extends Tipo {
  def mataA : List[Tipo] = List()
}


