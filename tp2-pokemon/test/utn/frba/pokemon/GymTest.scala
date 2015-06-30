package utn.frba.pokemon

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import org.junit.Test
import utn.frba.pokemon._
import org.junit.Before
import org.junit.runner.RunWith

// En esta clase se testea el simulador, rutinas, analizador de rutinas

class GymTest {
  // Setup 
  val ataqueNulo = Ataque(Electrico, 2, 3, (p: Pokemon) => p)
  val impactTrueno = Ataque(Electrico, 1, 3, (p: Pokemon) => p.subirExperiencia(3))
  val pikachu = Especie(id = 1, resistenciaEvolutiva = 10, pesoMaximo = 0, energiaMaximaInc = 51, pesoInc = 0, fuerzaInc = 0, velocidadInc = 0,
    tipoPrincipal = Electrico, tipoSecundario = None, evolucion = None, condicionEvolucion = None)

  val unPikachu = Pokemon(experiencia = 0, genero = Masculino, energia = 0, pesoExtra = 0, fuerzaExtra = 0, velocidadExtra = 0, especie = pikachu, estado = None,
    ataques = List(impactTrueno,ataqueNulo))

  @Test
  def `PokemonUsaPocion` = {
    val resultado = Simulador.realizarActividad(unPikachu, usarPocion)
    assertEquals(unPikachu.energia + 50, resultado.get.energia)
  }

  @Test
  def `PokemonAtacaConUnAtaqueQueCambiaSuEstado` = {
    val resultado = Simulador.realizarActividad(unPikachu, descansar)
    assertEquals(resultado.get.estado, Some(Dormido(0)))
  }

  @Test
  def `PokemonAtacaConUnAtaqueQueNoCambiaDeEstado` = {
    val resultado = Simulador.realizarActividad(unPikachu, realizarAtaque(ataqueNulo))
    assertEquals(unPikachu.estado, resultado.get.estado)
  }
 
/*
  @Test
  def `PokemonAtacaConUnAtaqueQueCambiaSuEstado` = {
    val resul = Simulador.entrenar(unCharizard, RealizarUnAtaque(reposar))
    assert((resul.isInstanceOf[EstadoDormido]) && (resul.pokemon.energia == resul.pokemon.energiaMaxima))
  }

  @Test
  def `PokemonPuedeTomar3Pociones` = {
    val resul = Simulador.entrenar(unCharizard, UsarPocion, UsarPocion, UsarPocion)
    assertEquals(unCharizard.energia + 150, resul.pokemon.energia)
  }

  @Test
  def `SeAceptanRutinasVacias` = {
    val resul = Simulador.entrenar(unCharizard)
    assert((unCharizard == resul.pokemon) && (resul.isInstanceOf[EstadoNormal]))
  }

  @Test
  def `ElPokemonGanaExperienciaAlAtacar` = {
    val resul = Simulador.entrenar(unCharizard, RealizarUnAtaque(enfocarse))
    assertEquals(unCharizard.experiencia + 50, resul.pokemon.experiencia)
  }

  @Test
  def `Un pokemon KO no realiza ninguna actividad` = {
    val resul = Simulador.entrenar(unCharizard, RealizarUnAtaque(explosion), UsarPocion, UsarPocion, UsarPocion)
    assertEquals(unCharizard.energia, resul.pokemon.energia)
  }

  @Test
  def `Si un pokemon no sabe la actividad no la ejecuta` = {
    val resul = Simulador.entrenar(unCharizard, RealizarUnAtaque(mordida))
    assert(resul.isInstanceOf[EstadoActividadNoEjecutada])
  }

  @Test
  def `Un pokemon puede levantar pesas` = {
    val kilosLevantados = 2
    val resul = Simulador.entrenar(unCharizard, LevantarPesas(kilosLevantados))
    assertEquals(unCharizard.experiencia + kilosLevantados, resul.pokemon.experiencia)
  }

  @Test
  def `Un pokemon puede nadar` = {
    val minutosNadados = 2
    val resul = Simulador.entrenar(unSquartle, Nadar(minutosNadados))
    assertEquals(unSquartle.energia - minutosNadados, resul.pokemon.energia)
    assertEquals(unSquartle.experiencia + 200, resul.pokemon.experiencia)
    assertEquals(unSquartle.velocidad + (minutosNadados % 60), resul.pokemon.velocidad)

  }
*/
}
