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

  var charizardEspecie = Especie(1,350, 0, 0, 0, 0, 0, Fuego, Pelea)
  var squartleEspecie = Especie(resistenciaEvolutiva = 100, pesoMaximo = 10.0, energiaMaximaInc = 12, tipoPrincipal = Agua, tipoSecundario = Normal)

  val reposar = AtaqueConEstado(Fuego, 0, 20, (e: Estado) => Dormido(e.pokemon.copy(energia = e.pokemon.energiaMaxima)))
  val enfocarse = AtaqueSinEstado(Fuego, 0, 30, (p: Pokemon) => p.copy(velocidad = p.velocidad + 1))
  val explosion = AtaqueConEstado(Fuego, 0, 20, (e: Estado) => KO(e.pokemon, "Pokemon KO"))
  val mordida = AtaqueSinEstado(Normal, 0, 30, (p: Pokemon) => p.copy(fuerza = p.fuerza + 1))
  val chorroDeAguaDelRiachuelo = AtaqueConEstado(Agua,0,10, (e: Estado) => Envenenado(e.pokemon.copy(fuerza = e.pokemon.fuerza + 1)))
  
  val unSquartle = Pokemon(nivel = 1, experiencia = 0, especie = squartleEspecie, fuerza = 2, ataques = List(mordida,chorroDeAguaDelRiachuelo))
  val unCharizard = Pokemon(nivel = 1, experiencia = 0, especie = charizardEspecie, fuerza = 6, ataques = List(enfocarse, reposar, explosion))

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
    assert((resul.isInstanceOf[Dormido]) && (resul.pokemon.energia == resul.pokemon.energiaMaxima))
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
    assert(resul.isInstanceOf[KO])
  }

  @Test
  def `CharizardSacaMusculos` = {
    val kilosLevantados = 2
    val resul = Simulador.entrenar(unCharizard, LevantarPesas(kilosLevantados))
    assertEquals(unCharizard.experiencia + 2 * kilosLevantados, resul.pokemon.experiencia)
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
    assert(resul.isInstanceOf[KO])
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
