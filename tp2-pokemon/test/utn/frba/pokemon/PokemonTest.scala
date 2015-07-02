package utn.frba.pokemon

import scala.util.{Try, Success, Failure}
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.assertFalse
import org.junit.Test
import org.junit.Ignore

// En esta clase se testea el modelo de pokemon y actividades individualmente

class EvolucionTest {

  @Test
  def `Crear Pokemon con valores invalidos` = {
    val poke = Pokemon.make(fuerzaExtra = 110, especie = Charizard, ataques = List(MordidaAtaque))
    assertTrue(poke.isFailure)
  }
  
  @Test
  def `Pokemon calcula su nivel en base a la experiencia` = {
    var pokemon = Pokemon.make(experiencia = 350, especie = Charmander).get

    assertEquals(2, pokemon.nivel)
    assertEquals(350, pokemon.experiencia)
  }
  
  @Test
  def `Pokemon calcula sus atributos dinamicamente` = {
    var pokemon = Pokemon.make(experiencia = 1050, especie = Charmander).get

    assertEquals(3, pokemon.nivel)
    assertEquals(30, pokemon.energiaMaxima)
    assertEquals(1.5, pokemon.peso, 0)
    assertEquals(9, pokemon.fuerza)
    assertEquals(3, pokemon.velocidad)
  }
  
  @Test
  def `Pokemon aplica incrementos retroactivamente` = {
    //Primera evolucion usa los incrementos de Charmander
    var pokemon = Pokemon.make(experiencia = 0, especie = Charmander).get

    assertEquals(Charmander, pokemon.especie)
    assertEquals(1, pokemon.nivel)
    assertEquals(Charmander.energiaMaximaInc, pokemon.energiaMaxima)
    assertEquals(Charmander.pesoInc, pokemon.peso, 0)
    assertEquals(Charmander.fuerzaInc, pokemon.fuerza)
    assertEquals(Charmander.velocidadInc, pokemon.velocidad)
    
    //Evolucion 1
    pokemon =  pokemon.cambiarExperiencia(pokemon.experienciaNivel(5))
    assertEquals(Charmeleon, pokemon.especie)
    assertEquals(5, pokemon.nivel)
    assertEquals(Charmeleon.energiaMaximaInc * 5, pokemon.energiaMaxima)
    assertEquals(Charmeleon.pesoInc * 5, pokemon.peso, 0)
    assertEquals(Charmeleon.fuerzaInc * 5, pokemon.fuerza)
    assertEquals(Charmeleon.velocidadInc * 5, pokemon.velocidad)
    
    //Evolucion 2

    pokemon =  pokemon.cambiarExperiencia(pokemon.experienciaNivel(20))
    assertEquals(Charizard, pokemon.especie)
    assertEquals(20, pokemon.nivel)
    assertEquals(Charizard.energiaMaximaInc * 20, pokemon.energiaMaxima)
    assertEquals(Charizard.pesoInc * 20, pokemon.peso, 0)
    assertEquals(Charizard.fuerzaInc * 20, pokemon.fuerza)
    assertEquals(Charizard.velocidadInc * 20, pokemon.velocidad)
  
  }
  
  @Test
  def `Pokemon KO no puede realizar una actividad` = {

    //Tipo de ataque es igual al tipo princial del pokemon.
    var pokemon =  Pokemon.make(estado = EstadoKO("KO"), experiencia = 1000, especie = Charmander).get

    val resultado = UsarPocion(pokemon)
    
    assertTrue(resultado.isFailure)

  }

  @Test
  def `Pokemon gana 50 de experiencia y sube de nivel.` = {

    //Tipo de ataque es igual al tipo princial del pokemon.
    var ataque =  MordidaAtaque
    var pokemon =  Pokemon.make(experiencia = 1000, especie = Charmander, ataques = List(ataque)).get
    var actividad = RealizarUnAtaque(ataque)

    val resultado = actividad(pokemon).get

    assertEquals(3, resultado.nivel)
    assertEquals(1050, resultado.experiencia)
  }

  @Test
  def `Evoluciona al subir nivel dada la experiencia inicial.` {
    var pokemon = Pokemon.make(especie = Charmander, experiencia = 999999).get
      
    assertEquals(12, pokemon.nivel)
    assertEquals(Charmeleon, pokemon.especie)
  }

  @Test
  def `Evolucionar por usar piedra lunar` {
    var pokemon = Pokemon.make(especie = Nidorina).get
    val resultado = UsarPiedra(PiedraLunar())(pokemon).get

    assertEquals(1, resultado.nivel)
    assertEquals(Nidoqueen, resultado.especie)
  }

  @Test
  def `Evolucionar por usar piedra del mismo tipo` {
    var pokemon = Pokemon.make(especie = Poliwhirl).get

    pokemon = UsarPiedra(PiedraEvolutivaComun(Agua))(pokemon).get

    assertTrue(pokemon.tipoPrincipal == Agua)
    assertEquals(Poliwrath, pokemon.especie)
  }
  
  @Test
  def `Evolucionar por intercambio` {
    var pokemon = Pokemon.make(especie = Squirtle).get

    pokemon = FingirIntercambio(pokemon).get

    assertEquals(Wartortle, pokemon.especie)
  }

  @Test
  def `Usar piedra envenenadora` {
    var pokemon = Pokemon.make(especie = Poliwhirl).get

    var piedra = PiedraEvolutivaComun(Electrico)

    val resultado = UsarPiedra(piedra)(pokemon).get

    assertEquals(EstadoEnvenenado, resultado.estado)
  }

  @Test
  def `Actividad deja valores invalidos en pokemon.` {
    var pokemon = Pokemon.make(deltaVelocidad = 99, especie = Poliwhirl).get
    var pokemonResultado = ComerCalcio(pokemon)
     
    assertTrue(pokemonResultado.isFailure)
  }
  
  @Test
  def `AlAtacarBajanLosPA` = {
    val ataque = Ataque(Agua, 1, 3, (p: Pokemon) => Try(p.cambiarExperiencia(3)))
    val unSquartle = Pokemon.make(experiencia = 0, especie = Squirtle, fuerzaExtra = 2, ataques = List(ataque)).get
    val paInicial = unSquartle.ataques.find((a) => a == ataque).get.puntosDeAtaque
    val resultado = RealizarUnAtaque(ataque)(unSquartle).get

    assertEquals(paInicial - 1, resultado.ataques.find((a) => a == ataque.bajarPA).get.puntosDeAtaque)
  }
  
}