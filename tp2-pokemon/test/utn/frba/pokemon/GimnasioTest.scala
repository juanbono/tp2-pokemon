package utn.frba.pokemon

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import org.junit.Test
import utn.frba.pokemon._
import org.junit.Before
import org.junit.runner.RunWith

class GimnasioTest {

  val reposar = new ReporsarAtaque 
  val enfocarse = new EnfocarseAtaque
  val explosion = new ExplosionAtaque
  val mordida = new MordidaAtaque
  val chorroDeAguaDelRiachuelo = new ChorroDeAguaDelRiachueloAtaque
  
  val unSquartle = Pokemon(nivel = 1, experiencia = 0, especie = Squirtle, fuerza = 2, ataques = List(mordida, chorroDeAguaDelRiachuelo))
  val unCharizard = Pokemon(nivel = 1, experiencia = 0, especie = Charizard, fuerza = 6, ataques = List(enfocarse, reposar, explosion))

  @Test
  def `PokemonUsaPocion` = {
    val resul = Simulador.entrenar(unCharizard, UsarPocion)
    assertEquals(unCharizard.energia + 50, resul.pokemon.energia)
  }

  @Test
  def `PokemonAtacaConUnAtaqueQueNoCambiaDeEstado` = {
    val resul = Simulador.entrenar(unCharizard, RealizarUnAtaque(enfocarse))
    assertEquals(unCharizard.velocidad + 1, resul.pokemon.velocidad)
  }

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
  def `PokemonKONoTomaPocion` = {
    val resul = Simulador.entrenar(unCharizard, RealizarUnAtaque(explosion), UsarPocion, UsarPocion, UsarPocion)
    assertEquals(unCharizard.energia, resul.pokemon.energia)
  }

  @Test
  def `CharizardNoSabeTodo` = {
    val resul = Simulador.entrenar(unCharizard, RealizarUnAtaque(mordida))
    assert(resul.isInstanceOf[EstadoActividadNoEjecutada])
  }

  @Test
  def `CharizardSacaMusculos` = {
    val kilosLevantados = 2
    val resul = Simulador.entrenar(unCharizard, LevantarPesas(kilosLevantados))
    assertEquals(unCharizard.experiencia +  kilosLevantados, resul.pokemon.experiencia)
  }

  @Test
  def `SquartleNadaPiola` = {
    val minutosNadados = 2
    val resul = Simulador.entrenar(unSquartle, Nadar(minutosNadados))
    assertEquals(unSquartle.energia - minutosNadados, resul.pokemon.energia)
    assertEquals(unSquartle.experiencia + 200, resul.pokemon.experiencia)
    assertEquals(unSquartle.velocidad + (minutosNadados % 60), resul.pokemon.velocidad)

  }

  @Test
  def `ChariNoNadesQueTeMoris` = {
    val minutosNadados = 2
    val resul = Simulador.entrenar(unCharizard, Nadar(minutosNadados))
    assert(resul.isInstanceOf[EstadoKO])
  }

  @Test
  def `DormidoNoSeEntrena` = {
    val resul = Simulador.entrenar(unCharizard, RealizarUnAtaque(reposar), UsarPocion, UsarPocion,UsarPocion)
    assertEquals(unCharizard.energiaMaxima, resul.pokemon.energia)
  }

  @Test
  def `SiEsBayerEsBueno` = {
    val resul = Simulador.entrenar(unSquartle,RealizarUnAtaque(chorroDeAguaDelRiachuelo),UsarAntidoto)
    assert(resul.isInstanceOf[EstadoNormal])
  }
}
