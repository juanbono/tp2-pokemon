package utn.frba.pokemon

trait Tipo {
  def tiposPeores : List[Tipo]
  def mataA(tipo : Tipo) : Boolean = tiposPeores.contains(tipo)
}

object Fuego extends Tipo {def tiposPeores = List(Planta, Hielo, Bicho)}
object Agua extends Tipo {def tiposPeores = List(Fuego, Tierra, Roca)}
object Planta extends Tipo {def tiposPeores = List(Agua, Tierra, Roca)}
object Tierra extends Tipo {def tiposPeores = List(Fuego, Electrico, Veneno, Roca)}
object Hielo extends Tipo {def tiposPeores = List(Planta, Tierra, Volador, Dragon)}
object Roca extends Tipo {def tiposPeores = List(Fuego, Hielo, Volador, Bicho)}
object Electrico extends Tipo {def tiposPeores = List(Agua, Volador)}
object Psiquico extends Tipo {def tiposPeores = List(Pelea, Veneno)}
object Pelea extends Tipo {def tiposPeores = List(Normal, Hielo, Roca)}
object Fantasma extends Tipo {def tiposPeores = List(Fantasma, Psiquico)}
object Volador extends Tipo {def tiposPeores = List(Planta, Pelea, Bicho)}
object Bicho extends Tipo {def tiposPeores = List(Planta, Psiquico)}
object Veneno extends Tipo {def tiposPeores : List[Tipo] = List(Planta, Hielo, Bicho)}
object Dragon extends Tipo {def tiposPeores = List(Dragon)}
object Normal extends Tipo {def tiposPeores = List()}
