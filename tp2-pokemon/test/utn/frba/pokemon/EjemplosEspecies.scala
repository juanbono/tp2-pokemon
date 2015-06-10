package utn.frba.pokemon

object Charmander extends Especie (
  id = 4,
  tipoPrincipal = Fuego,
  evolucion = Some(Charmeleon),
  condicionEvolucion = Some(new SubirNivelEvolucion(16))
) 

object Charmeleon extends Especie (
  id = 5,
  tipoPrincipal = Fuego,
  evolucion = Some(Charizard),
  condicionEvolucion = Some(new SubirNivelEvolucion(36))
)

object Charizard extends Especie (
  id = 6,
  tipoPrincipal = Fuego
)

object Nidorin extends Especie (
  id = 30,
  tipoPrincipal = Veneno,
  evolucion = Some(Nidorina),
  condicionEvolucion = Some(new SubirNivelEvolucion(16))
)

object Nidorina extends Especie (
  id = 31,
  tipoPrincipal = Veneno,
  evolucion = Some(Nidoqueen),
  condicionEvolucion = Some(new UsarPiedraLunarEvolucion)
)

object Nidoqueen extends Especie (
  id = 32,
  tipoPrincipal = Veneno,
  tipoSecundario = Some(Tierra)
)

object Poliwag extends Especie (
  id = 60,
  tipoPrincipal = Agua,
  evolucion = Some(Poliwhirl),
  condicionEvolucion = Some(new SubirNivelEvolucion(25))
)

object Poliwhirl extends Especie (
  id = 61,
  tipoPrincipal = Agua,
  evolucion = Some(Poliwrath),
  condicionEvolucion = Some(new UsarPiedraEvolucion)
)

object Poliwrath extends Especie (
  id = 62,
  tipoPrincipal = Agua,
  tipoSecundario = Some(Pelea)
)