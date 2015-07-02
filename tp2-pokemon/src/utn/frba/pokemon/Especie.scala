package utn.frba.pokemon

case class Especie (
    val id: Int = 0, // MissingNo.
    val resistenciaEvolutiva: Int = 100,
    val pesoMaximo: Double = 40,
    val energiaMaximaInc: Int = 10,
    val pesoInc: Double = 0.1,
    val fuerzaInc: Int = 1,
    val velocidadInc: Int = 1,
    val tipoPrincipal: Tipo,
    val tipoSecundario: Option[Tipo] = None,
    val evolucion: Option[Especie] = None,
    val condicionEvolucion: Option[CondicionEvolucion] = None
)

// Aca se definen todas las especies conocidas.
 
object Charmander extends Especie (
    id = 4, 
    resistenciaEvolutiva = 350,
    energiaMaximaInc = 10,
    pesoInc = 0.5,
    fuerzaInc = 3,
    velocidadInc = 1, 
    tipoPrincipal = Fuego,
    evolucion = Some(Charmeleon),
    condicionEvolucion = Some(new SubirNivelEvolucion(5))
    )

object Charmeleon extends Especie (
    id = 5, 
    resistenciaEvolutiva = 500,
    energiaMaximaInc = 20,
    pesoInc = 1,
    fuerzaInc = 4,
    velocidadInc = 2,
    tipoPrincipal = Fuego, 
    evolucion = Some(Charizard), 
    condicionEvolucion = Some(new SubirNivelEvolucion(20))
    )

object Charizard extends Especie (
    id = 6,
    resistenciaEvolutiva = 1000,
    energiaMaximaInc = 50,
    pesoInc = 2,
    fuerzaInc = 5,
    velocidadInc = 5,
    tipoPrincipal = Fuego
    )

object Nidorin extends Especie ( id = 30, tipoPrincipal = Veneno, evolucion = Some(Nidorina), condicionEvolucion = Some(new SubirNivelEvolucion(16)))
object Nidorina extends Especie (id = 31, tipoPrincipal = Veneno, evolucion = Some(Nidoqueen), condicionEvolucion = Some(new UsarPiedraLunarEvolucion))
object Nidoqueen extends Especie (id = 32, tipoPrincipal = Veneno, tipoSecundario = Some(Tierra))

object Poliwag extends Especie (id = 60, tipoPrincipal = Agua, evolucion = Some(Poliwhirl), condicionEvolucion = Some(new SubirNivelEvolucion(25)))
object Poliwhirl extends Especie (id = 61, tipoPrincipal = Agua, evolucion = Some(Poliwrath), condicionEvolucion = Some(UsarPiedraEvolucion))
object Poliwrath extends Especie (id = 62, tipoPrincipal = Agua, tipoSecundario = Some(Pelea))

object Squirtle extends Especie (id = 7, resistenciaEvolutiva = 220, tipoPrincipal = Agua, evolucion = Some(Wartortle), condicionEvolucion = Some(IntercambiarEvolucion))
object Wartortle extends Especie (id = 8, resistenciaEvolutiva = 300,  tipoPrincipal = Agua, evolucion = Some(Blastoise), condicionEvolucion = None)
object Blastoise extends Especie (id = 9, resistenciaEvolutiva = 400, tipoPrincipal = Agua, evolucion = None, condicionEvolucion = None)

object Misdreavus extends Especie (id = 200, resistenciaEvolutiva = 220, tipoPrincipal = Fantasma, energiaMaximaInc = 100, condicionEvolucion = None)
