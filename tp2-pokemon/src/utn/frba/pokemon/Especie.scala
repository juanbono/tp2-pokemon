package utn.frba.pokemon

case class Especie(
    resistenciaEvolutiva: Int = 0,
    pesoMaximo: Double = 0.0,
    energiaMaximaInc: Int = 0,
    pesoInc: Int = 0,
    fuerzaInc: Int = 0,
    velocidadInc: Int = 0,
    tipoPrincipal: Tipo,
    tipoSecundario: Tipo,
    especieAEvololucionar: Especie)
{
    
}