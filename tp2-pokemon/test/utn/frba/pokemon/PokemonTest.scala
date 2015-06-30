/*
package utn.frba.pokemon

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.assertFalse
import org.junit.Test
import org.junit.Ignore

// En esta clase se testea el modelo de pokemon y actividades individualmente

class EvolucionTest {

  // Setup 
  val impactTrueno = Ataque(electrico, 1, 3, (p: Pokemon) => p.subirExperiencia(3))
  val pikachu = Especie(id = 1,resistenciaEvolutiva = 10,pesoMaximo = 0,energiaMaximaInc = 0,pesoInc = 0,fuerzaInc = 0,velocidadInc = 0,
      tipoPrincipal = electrico,tipoSecundario = None,evolucion = None,condicionEvolucion = None)

  val unPikachu = Pokemon(experiencia = 0,genero = Masculino,energia = 0,pesoExtra = 0,fuerzaExtra = 0,velocidadExtra = 0,especie = pikachu,estado = None,
    ataques = List(impactTrueno))
    
  @Test
  val unPikachuInvalido = Pokemon(experiencia = -10,genero = Masculino,energia = -10,pesoExtra = -10,fuerzaExtra = 0,velocidadExtra = 0,especie = pikachu,estado = None,
    ataques = List(impactTrueno))

    assertFalse(unPikachuInvalido.esPokemonValido) // fijarse este test.
  }

  @Test
  def `Pokemon gana 50 de experiencia y sube de nivel.` = {

    //Tipo de ataque es igual al tipo princial del pokemon.
    var ataque = new MordidaAtaque(puntosDeAtaque = 1, maximoInicialPA = 10)
    var pokemon = new Pokemon(nivel = 1, experiencia = 300, especie = Charizard, ataques = List(ataque))
    var actividad = new RealizarUnAtaque(ataque)

    val resultado = actividad.ejecutar(pokemon)

    assertEquals(3, resultado.pokemon.nivel)
    assertEquals(350, resultado.pokemon.experiencia)
  }

  @Test
  def `Evolucionar por subir de nivel` {
    var pokemon = Pokemon(nivel = 4, especie = Charmander, experiencia = 999999)

    pokemon = pokemon.subirNivel

    assertEquals(5, pokemon.nivel)
    assertEquals(pokemon.especie, Charmeleon)
  }

  @Test
  def `Evolucionar por usar piedra lunar` {
    var pokemon = Pokemon(
      nivel = 25,
      especie = Nidorina)

    pokemon = Simulador.entrenar(pokemon, UsarPiedra(PiedraLunar())).fold(null)(p => p)

    assertEquals(pokemon.nivel, 25)
    assertEquals(pokemon.especie, Nidoqueen)
  }

  @Test
  def `Evolucionar por usar piedra del mismo tipo` {
    var pokemon = Pokemon(
      nivel = 25,
      especie = Poliwhirl)

    pokemon = Simulador.entrenar(pokemon, UsarPiedra(PiedraEvolutivaComun(Agua))).fold(null)(p => p)

    assertEquals(pokemon.nivel, 25)
    assertEquals(pokemon.especie, Poliwrath)
  }

  @Test
  def `Usar piedra envenenadora` {
    var pokemon = Pokemon(nivel = 25, especie = Poliwhirl)

    var piedra = PiedraEvolutivaComun(Electrico)

    val resultado = Simulador.entrenar(pokemon, UsarPiedra(piedra))

    assertEquals(EstadoEnvenenado(resultado.pokemon), resultado)
  }

  @Test
  def `Actividad deja valores invalidos en pokemon` {
    var pokemon = Pokemon(nivel = 1, velocidad = 99, especie = Poliwhirl)
    var resultado = Simulador.entrenar(pokemon, ComerCalcio)

    assertEquals(resultado, EstadoActividadNoEjecutada(resultado.pokemon, List("La actividad produce estado invalido", "velocidad debe ser un numero de 1 a 100").toString()))

  }
  @Test
  def `AlAtacarBajanLosPA` = {
    val ataque = Ataque(agua, 1, 3, (p: Pokemon) => p.subirExperiencia(3))
    val unSquartle = Pokemon(nivel = 1, experiencia = 0, especie = Squirtle, fuerza = 2, ataques = List(ataque))
    val paInicial = unSquartle.ataques.find((a) => a == ataque).get.puntosDeAtaque
    val resultado = Simulador.entrenar(unSquartle, RealizarUnAtaque(ataque))

    assertEquals(paInicial - 1, resultado.pokemon.ataques.find((a) => a == ataque.bajarPA).get.puntosDeAtaque)
  }
}
*/