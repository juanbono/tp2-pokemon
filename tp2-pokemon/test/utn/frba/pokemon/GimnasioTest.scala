package utn.frba.pokemon

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import org.junit.Test
import utn.frba.pokemon._
import org.junit.Before
import org.junit.runner.RunWith
import scala.util.{Try, Success, Failure}

// En esta clase se testea el simulador, rutinas, analizador de rutinas

class GimnasioTest {

  val reposar =  ReporsarAtaque
  val enfocarse = EnfocarseAtaque
  val explosion = ExplosionAtaque
  val mordida =  MordidaAtaque
  val chorroDeAguaDelRiachuelo = ChorroDeAguaDelRiachueloAtaque

  val unSquartle = Pokemon.make(energia = 9, energiaMaximaExtra = 150, especie = Squirtle, fuerzaExtra = 2, ataques = List(mordida, chorroDeAguaDelRiachuelo)).get
  val unCharizard = Pokemon.make(experiencia = 0, energiaMaximaExtra = 150, especie = Charizard, fuerzaExtra = 6, ataques = List(enfocarse, reposar, explosion)).get

  @Test
  def `PokemonUsaPocion` = {
    val resul = entrenarPokemon(unCharizard, UsarPocion).get
    assertEquals(unCharizard.energia + 50, resul.energia)
  }

  @Test
  def `PokemonAtacaConUnAtaqueQueNoCambiaDeEstado` = {
    val resul = entrenarPokemon(unCharizard, RealizarUnAtaque(enfocarse)).get
    assertEquals(unCharizard.velocidad + 1, resul.velocidad)
  }

  @Test
  def `PokemonAtacaConUnAtaqueQueCambiaSuEstado` = {
    val resul = entrenarPokemon(unCharizard, RealizarUnAtaque(reposar)).get
    assert((resul.estado.isInstanceOf[EstadoDormido]) && (resul.energia == resul.energiaMaxima))
  }

  @Test
  def `PokemonPuedeTomar3Pociones` = {
    val resul = entrenarPokemon(unCharizard, UsarPocion, UsarPocion, UsarPocion).get
    assertEquals(unCharizard.energia + 150, resul.energia)
  }

  @Test
  def `SeAceptanRutinasVacias` = {
    val resul = entrenarPokemon(unCharizard).get
    assert((unCharizard == resul) && (resul.estado == EstadoNormal))
  }

  @Test
  def `ElPokemonGanaExperienciaAlAtacar` = {
    val resul = entrenarPokemon(unCharizard, RealizarUnAtaque(enfocarse)).get
    assertEquals(unCharizard.experiencia + 50, resul.experiencia)
  }

  @Test
  def `Un pokemon KO no realiza ninguna actividad y arroja excepcion` = {
    val resul = entrenarPokemon(unCharizard, RealizarUnAtaque(explosion), UsarPocion, UsarPocion, UsarPocion)
    assertTrue(resul.isFailure)
    try {
      resul.get
      fail("Deberia lanzar KOException porque el pokemon quedo KO y se le pidio hacer mas actividades");
    } catch {
        case KOException(_) => 
        case _ : Throwable => fail("Deberia lanzar KOException porque el pokemon quedo KO y se le pidio hacer mas actividades");
    }
  }

  @Test
  def `Pokemon no conoce el ataque, lanza excepcion al intentar ejecutarlo` = {
    val resul = entrenarPokemon(unCharizard, RealizarUnAtaque(mordida))
    
    assertTrue(resul.isFailure)
  }

  @Test
  def `Un pokemon puede levantar pesas` = {
    val kilosLevantados = 2
    val resul = entrenarPokemon(unCharizard, LevantarPesas(kilosLevantados)).get
    assertEquals(unCharizard.experiencia + kilosLevantados, resul.experiencia)
  }

  @Test
  def `Un pokemon puede nadar` = {
    val minutosNadados = 2
    val resul = entrenarPokemon(unSquartle, Nadar(minutosNadados)).get
    assertEquals(unSquartle.energia - minutosNadados, resul.energia)
    assertEquals(unSquartle.experiencia + 200, resul.experiencia)
    assertEquals(unSquartle.velocidad + (minutosNadados % 60), resul.velocidad)

  }

  @Test
  def `Un pokemon de fuego queda KO al nadar` = {
    val minutosNadados = 2
    val resul = entrenarPokemon(unCharizard, Nadar(minutosNadados)).get
    assert(resul.estado.isInstanceOf[EstadoKO])
  }

  @Test
  def `Un pokemon dormido ignora 3 actividades` = {
    val resul = entrenarPokemon(unCharizard, RealizarUnAtaque(reposar), UsarPocion, UsarPocion, UsarPocion).get
    assertEquals(unCharizard.energiaMaxima, resul.energia)
  }
  
  @Test
  def `Un pokemon dormido ignora 3 actividades pero se despierta en la 4ta` = {
    val resul = entrenarPokemon(unCharizard, RealizarUnAtaque(reposar), UsarPocion, UsarPocion, UsarPocion, ComerHierro).get
    assertEquals(unCharizard.energiaMaxima, resul.energia)
    assertEquals(unCharizard.fuerza + 5, resul.fuerza)
  }

  @Test
  def `Un pokemon envenenado se cura al tomar antidoto` = {
    val resul = entrenarPokemon(unSquartle, RealizarUnAtaque(chorroDeAguaDelRiachuelo), UsarAntidoto).get
    assert(resul.estado == EstadoNormal)
  }

  //Tests con rutinas
  @Test
  def `Rutina que ejecuta normalmente` = {
    val pokemon = Pokemon.make(experiencia = 0, energia = 0, energiaMaximaExtra = 100, especie = Charizard, fuerzaExtra = 10).get
    val rutina = new Rutina(nombre = "Rutina 1", actividades = UsarPocion, UsarPocion, ComerHierro)
    val resultado = rutina.ejecutar(pokemon)

    assertEquals(EstadoNormal, resultado.get.estado)
    assertEquals(100, resultado.get.energia)
    assertEquals(Charizard.fuerzaInc * pokemon.nivel + 15, resultado.get.fuerza)
  }

  @Test
  def `Rutina que no puede ejecutar` = {
    val pokemon = Pokemon.make(experiencia = 0, especie = Charizard, fuerzaExtra = 10).get
    val rutina = new Rutina(nombre = "Rutina 1", actividades = RealizarUnAtaque(mordida))
    val resultado = rutina.ejecutar(pokemon)

    // no conoce el ataque
    assertTrue(resultado.isFailure)
  }

  //Tests Analizador de rutinas con criterios
  @Test
  def `Obtener mejor rutina que obtenga nivel mas alto para un pokemon` = {
    
    val mayorNivelCriterio = (p1: Pokemon, p2: Pokemon) => p1.nivel > p2.nivel
    val pokemon = Pokemon.make(experiencia = 349, especie = Charizard, fuerzaExtra = 10, ataques = List(mordida)).get
    val rutina1 = new Rutina(nombre = "Rutina 1", actividades = RealizarUnAtaque(enfocarse)) //no tiene el ataque
    val rutina2 = new Rutina(nombre = "Rutina 2", actividades = RealizarUnAtaque(mordida)) //tiene el ataque, le sube experiencia
    val rutina3 = new Rutina(nombre = "Rutina 3", actividades = RealizarUnAtaque(explosion)) //no tiene el ataque

    assertEquals("Rutina 2", obtenerMejorRutina(pokemon, mayorNivelCriterio, rutina1, rutina2, rutina3))
  }

  @Test
  def `Obtener mejor rutina que obtenga el menor peso para un pokemon` = {

    val menorPesoCriterio = (p1: Pokemon, p2: Pokemon) => p1.peso < p2.peso
    val pokemon = Pokemon.make(genero = Femenino, pesoExtra = 30, especie = Charizard).get
    val rutina1 = new Rutina(nombre = "Rutina 1", actividades = FingirIntercambio)
    val rutina2 = new Rutina(nombre = "Rutina 2", actividades = FingirIntercambio, FingirIntercambio)
    val rutina3 = new Rutina(nombre = "Rutina 3", actividades = FingirIntercambio, FingirIntercambio, FingirIntercambio)

    assertEquals("Rutina 3", obtenerMejorRutina(pokemon, menorPesoCriterio, rutina1, rutina2, rutina3))
  }

  @Test
  def `Obtener mejor rutina que obtenga la mayor energia para un pokemon` = {

    val mayorEnergiaCriterio = (p1: Pokemon, p2: Pokemon) => p1.energia > p2.energia
    val pokemon = Pokemon.make(genero = Femenino, energia = 0, energiaMaximaExtra = 100, pesoExtra = 10, especie = Charizard).get
    val rutina1 = new Rutina(nombre = "Rutina 1", actividades = FingirIntercambio, UsarPocion)
    val rutina2 = new Rutina(nombre = "Rutina 2", actividades = FingirIntercambio, FingirIntercambio)
    val rutina3 = new Rutina(nombre = "Rutina 3", actividades = FingirIntercambio, FingirIntercambio, FingirIntercambio)

    assertEquals("Rutina 1", obtenerMejorRutina(pokemon, mayorEnergiaCriterio, rutina1, rutina2, rutina3))
  }

  @Test
  def `Obtener rutina que haga mas veloz a un pokemon ` = {
    val rutina1 = new Rutina("Rutina 1", UsarPocion, UsarPocion)
    val rutina2 = new Rutina("Rutina 2", Nadar(3), Nadar(3))
    val rutina3 = new Rutina("Rutina 3", Nadar(3))

    val criterio = (p1: Pokemon, p2: Pokemon) => p1.velocidad > p2.velocidad

    assertEquals("Rutina 2", obtenerMejorRutina(unSquartle, criterio, rutina1, rutina2, rutina3))
  }
  
  @Test
  def `Ninguna rutina puede ser realizada por el pokemon ` = {
    val ataqueSinPuntos = Ataque(puntosDeAtaque = 0, efecto = (p: Pokemon) => Try(p.cambiarEnergia(1)))
     
    val rutina1 = new Rutina("Rutina 1", RealizarUnAtaque(ataqueSinPuntos))
    val rutina2 = new Rutina("Rutina 2", Nadar(10))

    val criterio = (p1: Pokemon, p2: Pokemon) => p1.velocidad > p2.velocidad


    assertEquals("Ninguna rutina", obtenerMejorRutina(unSquartle, criterio, rutina1, rutina2))
  }

}
