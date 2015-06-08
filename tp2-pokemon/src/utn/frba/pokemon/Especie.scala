package utn.frba.pokemon

abstract class Especie {
    val resistenciaEvolutiva: Int
    val pesoMaximo: Double
    val energiaMaximaInc: Int
    val pesoInc: Double
    val fuerzaInc: Int
    val velocidadInc: Int
    val tipoPrincipal: Tipo
    val tipoSecundario: Option[Tipo]
    val especieAEvolucionar: Option[Especie]
}


case class Charizard (val resistenciaEvolutiva: Int = 350, val pesoMaximo: Double = 0.0, val energiaMaximaInc: Int = 0,
    val pesoInc: Double = 0.0, val fuerzaInc: Int = 0, val velocidadInc: Int = 0, val tipoPrincipal: Tipo = Fuego, 
    val tipoSecundario: Option[Tipo] = None, val especieAEvolucionar: Option[Especie] = Some(new Charmeleon)) extends Especie

case class Charmeleon (val resistenciaEvolutiva: Int = 400, val pesoMaximo: Double = 0.0, val energiaMaximaInc: Int = 0,
    val pesoInc: Double = 0.0, val fuerzaInc: Int = 0, val velocidadInc: Int = 0, val tipoPrincipal: Tipo = Fuego, 
    val tipoSecundario: Option[Tipo] = None, val especieAEvolucionar: Option[Especie] = Some(new Charmander)) extends Especie
    
case class Charmander (val resistenciaEvolutiva: Int = 500, val pesoMaximo: Double = 0.0, val energiaMaximaInc: Int = 0,
    val pesoInc: Double = 0.0, val fuerzaInc: Int = 0, val velocidadInc: Int = 0, val tipoPrincipal: Tipo = Fuego, 
    val tipoSecundario: Option[Tipo] = None, val especieAEvolucionar: Option[Especie] = None) extends Especie
    