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
    val resul = UsarPocion(unCharizard).get
    assertEquals(unCharizard.energia + 50, resul.energia)
  }

  @Test
  def `PokemonAtacaConUnAtaqueQueNoCambiaDeEstado` = {
    val resul =  RealizarUnAtaque(enfocarse)(unCharizard).get
    assertEquals(unCharizard.velocidad + 1, resul.velocidad)
  }

  @Test
  def `PokemonAtacaConUnAtaqueQueCambiaSuEstado` = {
    val resul = RealizarUnAtaque(reposar)(unCharizard).get
    assert((resul.estado.isInstanceOf[EstadoDormido]) && (resul.energia == resul.energiaMaxima))
  }

  @Test
  def `PokemonPuedeTomar3Pociones` = {
    val resul = realizarRutina(unCharizard, ("rutina1", List(UsarPocion, UsarPocion, UsarPocion))).get
    assertEquals(unCharizard.energia + 150, resul.energia)
  }

  @Test
  def `SeAceptanRutinasVacias` = {
    val resul = realizarRutina(unCharizard, ("rutina vacia", List())).get
    assert((unCharizard == resul) && (resul.estado == EstadoNormal))
  }

  @Test
  def `ElPokemonGanaExperienciaAlAtacar` = {
    val resul = RealizarUnAtaque(enfocarse)(unCharizard).get
    assertEquals(unCharizard.experiencia + 50, resul.experiencia)
  }

  @Test
  def `Un pokemon KO no realiza ninguna actividad y arroja excepcion` = {
    val resul = realizarRutina(unCharizard, ("rutina KO", List(RealizarUnAtaque(explosion), UsarPocion, UsarPocion, UsarPocion)))
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
    val resul = RealizarUnAtaque(mordida)(unCharizard)
    
    assertTrue(resul.isFailure)
  }

  @Test
  def `Un pokemon puede levantar pesas` = {
    val kilosLevantados = 2
    val resul = LevantarPesas(kilosLevantados)(unCharizard).get
    assertEquals(unCharizard.experiencia + kilosLevantados, resul.experiencia)
  }

  @Test
  def `Un pokemon puede nadar` = {
    val minutosNadados = 2
    val resul = Nadar(minutosNadados)(unSquartle).get
    assertEquals(unSquartle.energia - minutosNadados, resul.energia)
    assertEquals(unSquartle.experiencia + 200, resul.experiencia)
    assertEquals(unSquartle.velocidad + (minutosNadados % 60), resul.velocidad)

  }

  @Test
  def `Un pokemon de fuego queda KO al nadar` = {
    val minutosNadados = 2
    val resul = Nadar(minutosNadados)(unCharizard).get
    assert(resul.estado.isInstanceOf[EstadoKO])
  }

  @Test
  def `Un pokemon dormido ignora 3 actividades` = {
    val resul = realizarRutina(unCharizard, ("rutina1", List(RealizarUnAtaque(reposar), UsarPocion, UsarPocion, UsarPocion))).get
    assertEquals(unCharizard.energiaMaxima, resul.energia)
  }
  
  @Test
  def `Un pokemon dormido ignora 3 actividades pero se despierta en la 4ta` = {
    val resul = realizarRutina(unCharizard, ("rutina1", List(RealizarUnAtaque(reposar), UsarPocion, UsarPocion, UsarPocion, ComerHierro))).get
    assertEquals(unCharizard.energiaMaxima, resul.energia)
    assertEquals(unCharizard.fuerza + 5, resul.fuerza)
  }

  @Test
  def `Un pokemon envenenado se cura al tomar antidoto` = {
    val resul = realizarRutina(unSquartle, ("rutina", List(RealizarUnAtaque(chorroDeAguaDelRiachuelo), UsarAntidoto))).get
    assert(resul.estado == EstadoNormal)
  }

  //Tests con rutinas
  @Test
  def `Rutina que ejecuta normalmente` = {
    val pokemon = Pokemon.make(experiencia = 0, energia = 0, energiaMaximaExtra = 100, especie = Charizard, fuerzaExtra = 10).get
    val rutina = ("Rutina 1", List(UsarPocion, UsarPocion, ComerHierro))
    val resultado = realizarRutina(pokemon, rutina)

    assertEquals(EstadoNormal, resultado.get.estado)
    assertEquals(100, resultado.get.energia)
    assertEquals(Charizard.fuerzaInc * pokemon.nivel + 15, resultado.get.fuerza)
  }

  @Test
  def `Rutina que no puede ejecutar` = {
    val pokemon = Pokemon.make(experiencia = 0, especie = Charizard, fuerzaExtra = 10).get
    val rutina = ("Rutina 1", List( RealizarUnAtaque(mordida)))
    val resultado = realizarRutina(pokemon, rutina)

    // no conoce el ataque
    assertTrue(resultado.isFailure)
  }

  //Tests Analizador de rutinas con criterios
  @Test
  def `Obtener mejor rutina que obtenga nivel mas alto para un pokemon` = {
    
    val mayorNivelCriterio = (p1: Pokemon, p2: Pokemon) => p1.nivel > p2.nivel
    val pokemon = Pokemon.make(experiencia = 349, especie = Charizard, fuerzaExtra = 10, ataques = List(mordida)).get
    val rutina1 = ("Rutina 1", List(RealizarUnAtaque(enfocarse))) //no tiene el ataque
    val rutina2 = ("Rutina 2", List(RealizarUnAtaque(mordida))) //tiene el ataque, le sube experiencia
    val rutina3 = ( "Rutina 3", List(RealizarUnAtaque(explosion))) //no tiene el ataque

    assertEquals("Rutina 2", analizarRutinas(pokemon, mayorNivelCriterio, rutina1, rutina2, rutina3))
  }

  @Test
  def `Obtener mejor rutina que obtenga el menor peso para un pokemon` = {

    val menorPesoCriterio = (p1: Pokemon, p2: Pokemon) => p1.peso < p2.peso
    val pokemon = Pokemon.make(genero = Femenino, pesoExtra = 30, especie = Charizard).get
    val rutina1 = ("Rutina 1", List(FingirIntercambio))
    val rutina2 = ("Rutina 2", List(FingirIntercambio, FingirIntercambio))
    val rutina3 = ("Rutina 3", List(FingirIntercambio, FingirIntercambio, FingirIntercambio))

    assertEquals("Rutina 3", analizarRutinas(pokemon, menorPesoCriterio, rutina1, rutina2, rutina3))
  }

  @Test
  def `Obtener mejor rutina que obtenga la mayor energia para un pokemon` = {

    val mayorEnergiaCriterio = (p1: Pokemon, p2: Pokemon) => p1.energia > p2.energia
    val pokemon = Pokemon.make(genero = Femenino, energia = 0, energiaMaximaExtra = 100, pesoExtra = 10, especie = Charizard).get
    val rutina1 = ("Rutina 1", List(FingirIntercambio, UsarPocion))
    val rutina2 = ("Rutina 2", List(FingirIntercambio, FingirIntercambio))
    val rutina3 = ("Rutina 3", List(FingirIntercambio, FingirIntercambio, FingirIntercambio))

    assertEquals("Rutina 1", analizarRutinas(pokemon, mayorEnergiaCriterio, rutina1, rutina2, rutina3))
  }

  @Test
  def `Obtener rutina que haga mas veloz a un pokemon ` = {
    val rutina1 = ("Rutina 1", List(UsarPocion, UsarPocion))
    val rutina2 = ("Rutina 2", List(Nadar(3), Nadar(3)))
    val rutina3 = ("Rutina 3", List(Nadar(3)))

    val criterio = (p1: Pokemon, p2: Pokemon) => p1.velocidad > p2.velocidad

    assertEquals("Rutina 2", analizarRutinas(unSquartle, criterio, rutina1, rutina2, rutina3))
  }
  
  @Test
  def `Ninguna rutina puede ser realizada por el pokemon ` = {
    val ataqueSinPuntos = Ataque(puntosDeAtaque = 0, efecto = (p: Pokemon) => Try(p.cambiarEnergia(1)))
     
    val rutina1 = ("Rutina 1", List(RealizarUnAtaque(ataqueSinPuntos)))
    val rutina2 = ("Rutina 2", List(Nadar(10)))

    val criterio = (p1: Pokemon, p2: Pokemon) => p1.velocidad > p2.velocidad


    assertEquals("Ninguna rutina", analizarRutinas(unSquartle, criterio, rutina1, rutina2))
  }

}
