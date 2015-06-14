package utn.frba.pokemon
import utn.frba.pokemon._
case class Especie (
    val id: Int = 0, // MissingNo.
    val resistenciaEvolutiva: Int = 10,
    val pesoMaximo: Double = 0,
    val energiaMaximaInc: Int = 0,
    val pesoInc: Double = 0,
    val fuerzaInc: Int = 0,
    val velocidadInc: Int = 0,
    val tipoPrincipal: Tipo,
    val tipoSecundario: Option[Tipo] = None,
    val evolucion: Option[Especie] = None,
    val condicionEvolucion: Option[CondicionEvolucion] = None
)

// Aca se definen todas las especies conocidas.
 

object Charmander extends Especie (id = 4, resistenciaEvolutiva = 350, tipoPrincipal = Fuego, evolucion = Some(Charmeleon), condicionEvolucion = Some(new SubirNivelEvolucion(5)))
object Charmeleon extends Especie (id = 5, tipoPrincipal = Fuego, evolucion = Some(Charizard), condicionEvolucion = Some(new SubirNivelEvolucion(36)))
object Charizard extends Especie (id = 6, tipoPrincipal = Fuego)

object Nidorin extends Especie ( id = 30, tipoPrincipal = Veneno, evolucion = Some(Nidorina), condicionEvolucion = Some(new SubirNivelEvolucion(16)))
object Nidorina extends Especie (id = 31, tipoPrincipal = Veneno, evolucion = Some(Nidoqueen), condicionEvolucion = Some(new UsarPiedraLunarEvolucion))
object Nidoqueen extends Especie (id = 32, tipoPrincipal = Veneno, tipoSecundario = Some(Tierra))

object Poliwag extends Especie (id = 60, tipoPrincipal = Agua, evolucion = Some(Poliwhirl), condicionEvolucion = Some(new SubirNivelEvolucion(25)))
object Poliwhirl extends Especie (id = 61, tipoPrincipal = Agua, evolucion = Some(Poliwrath), condicionEvolucion = Some(new UsarPiedraEvolucion))
object Poliwrath extends Especie (id = 62, tipoPrincipal = Agua, tipoSecundario = Some(Pelea))

object Squirtle extends Especie (id = 7, tipoPrincipal = Agua, evolucion = Some(Wartortle), condicionEvolucion = None)
object Wartortle extends Especie (id = 8, tipoPrincipal = Agua, evolucion = Some(Blastoise), condicionEvolucion = None)
object Blastoise extends Especie (id = 9, tipoPrincipal = Agua, evolucion = None, condicionEvolucion = None)
