package utn.frba.pokemon

abstract class Especie {
    val resistenciaEvolutiva: Int
    val pesoMaximo: Double
    val energiaMaximaInc: Int
    val pesoInc: Double
    val fuerzaInc: Int
    val velocidadInc: Int
    val tipoPrincipal: TipoPokemon
    val tipoSecundario: Option[TipoPokemon]
    val evolucion: Option[Especie]
    val condicionEvolucion: Option[CondicionEvolucion]
}


case class Charizard (
    val resistenciaEvolutiva: Int = 350,
    val pesoMaximo: Double = 0.0,
    val energiaMaximaInc: Int = 0,
    val pesoInc: Double = 0.0,
    val fuerzaInc: Int = 0,
    val velocidadInc: Int = 0,
    val tipoPrincipal: TipoPokemon = Fuego, 
    val tipoSecundario: Option[TipoPokemon] = None,
    val evolucion: Option[Especie] = Some(new Charmeleon),
    val condicionEvolucion: Option[CondicionEvolucion] = Some(new SubirNivelEvolucion(15))
) extends Especie

case class Charmeleon (
    val resistenciaEvolutiva: Int = 400,
    val pesoMaximo: Double = 0.0,
    val energiaMaximaInc: Int = 0,
    val pesoInc: Double = 0.0,
    val fuerzaInc: Int = 0,
    val velocidadInc: Int = 0,
    val tipoPrincipal: TipoPokemon = Fuego, 
    val tipoSecundario: Option[TipoPokemon] = None,
    val evolucion: Option[Especie] = Some(new Charmander),
    val condicionEvolucion: Option[CondicionEvolucion] = Some(new SubirNivelEvolucion(30))
) extends Especie
    
case class Charmander (val resistenciaEvolutiva: Int = 500,
    val pesoMaximo: Double = 0.0, 
    val energiaMaximaInc: Int = 0,
    val pesoInc: Double = 0.0, 
    val fuerzaInc: Int = 0, 
    val velocidadInc: Int = 0, 
    val tipoPrincipal: TipoPokemon = Fuego, 
    val tipoSecundario: Option[TipoPokemon] = None, 
    val evolucion: Option[Especie] = None,
    val condicionEvolucion: Option[CondicionEvolucion] = None
) extends Especie
    