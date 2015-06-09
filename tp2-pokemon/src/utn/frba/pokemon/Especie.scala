package utn.frba.pokemon

case class Especie (
    val id: Int = 0, // MissingNo.
    val resistenciaEvolutiva: Int = 10,
    val pesoMaximo: Double = 0,
    val energiaMaximaInc: Int = 0,
    val pesoInc: Double = 0.0,
    val fuerzaInc: Int = 0,
    val velocidadInc: Int = 0,
    val tipoPrincipal: TipoPokemon,
    val tipoSecundario: Option[TipoPokemon] = None,
    val evolucion: Option[Especie] = None,
    val condicionEvolucion: Option[CondicionEvolucion] = None
)
