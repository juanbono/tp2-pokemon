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
  val explosion = Ataque(Electrico, 2, 3, (p: Pokemon) => p.cambiarEstado(Some(KO("El pokemon se autodestruyo de una explosion."))))
  val enfocarse = Ataque(Normal, 2, 3, (p: Pokemon) => p.cambiarVelocidad(1))
  val reposar = Ataque(Normal, 2, 3, (p: Pokemon) => p.copy(energia = p.energiaMaxima).cambiarEstado(Some(Dormido(0))))
  val ataqueNulo = Ataque(Normal, 2, 3, (p: Pokemon) => p)
  val impactTrueno = Ataque(Electrico, 1, 3, (p: Pokemon) => p.subirExperiencia(3))
  val pikachu = Especie(id = 1, resistenciaEvolutiva = 10, pesoMaximo = 0, energiaMaximaInc = 300, pesoInc = 0, fuerzaInc = 0, velocidadInc = 0,
    tipoPrincipal = Electrico, tipoSecundario = Some(Normal), evolucion = None, condicionEvolucion = None)

  val unPikachu = Pokemon(experiencia = 0, genero = Masculino, energia = 0, pesoExtra = 0, fuerzaExtra = 0, velocidadExtra = 0, especie = pikachu, estado = None,
    ataques = List(impactTrueno, ataqueNulo, reposar,enfocarse,explosion))

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

  @Test
  def `PokemonAtacaConAtaqueQueCambiaSuEstado` = {
    val resultado = Simulador.realizarActividad(unPikachu, realizarAtaque(reposar))
    assertEquals(Some(Dormido(0)), resultado.get.estado)
  }

  @Test
  def `PokemonPuedeTomar3Pociones` = {
    val rutina3Pociones = ("Rutina de 3 pociones", List(usarPocion, usarPocion, usarPocion))
    val resultado = Simulador.realizarRutina(unPikachu, rutina3Pociones)
    assertEquals(unPikachu.energia + 150, resultado.get.energia)
  }

  @Test
  def `ElPokemonGanaExperienciaAlAtacar` = {
    val resultado = Simulador.realizarActividad(unPikachu, realizarAtaque(enfocarse))
    assertEquals(unPikachu.experiencia + 20, resultado.get.experiencia)
  }

  @Test
  def `Un pokemon KO no realiza ninguna actividad` = {
    val rutinaSuicida = ("Rutina Suicida", List(realizarAtaque(explosion),usarPocion,usarPocion,usarPocion))
    val resultado = Simulador.realizarRutina(unPikachu, rutinaSuicida)
    assert(resultado.isFailure)
  }

  /*
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
  
    @Test
  def `SeAceptanRutinasVacias` = {
    val rutinaVacia = ("Rutina sin actividades", Nil)
    val resultado = Simulador.realizarRutina(unPikachu, rutinaVacia)
    assert(resultado.isFailure)
  }
*/
}