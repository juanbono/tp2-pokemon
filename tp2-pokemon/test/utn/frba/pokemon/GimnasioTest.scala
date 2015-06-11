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
  
  //Tests con rutinas
  @Test
  def `Rutina que ejecuta normalmente` = {
    val pokemon =  Pokemon(nivel = 1, experiencia = 0, especie = Charizard, fuerza = 10)
    val rutina = new Rutina(nombre="Rutina 1", actividades = UsarPocion, UsarPocion, ComerHierro)
    val resultado = rutina.ejecutar(pokemon)
    
    assertEquals(EstadoNormal(resultado.pokemon), resultado.estado)
    assertEquals(100, resultado.pokemon.energia)
    assertEquals(15, resultado.pokemon.fuerza)
  }
  

  @Test
  def `Rutina que no puede ejecutar` = {
    val pokemon =  Pokemon(nivel = 1, experiencia = 0, especie = Charizard, fuerza = 10)
    val rutina = new Rutina(nombre="Rutina 1", actividades = RealizarUnAtaque(mordida))
    val resultado = rutina.ejecutar(pokemon)
    
    assertEquals(EstadoActividadNoEjecutada(resultado.pokemon, "Pokemon no puede realizar ataque: No conoce el ataque %s".format(mordida.getClass.toString())), resultado.estado)
  }
  
  //Tests Analizador de rutinas con criterios
  @Test
  def `Obtener mejor rutina que obtenga nivel mas alto para un pokemon` = { 
    val mayorNivelCriterio = (r1 : ResultadoRutina, r2 : ResultadoRutina) => r1.pokemon.nivel > r2.pokemon.nivel
  
  //val menorPesolCriterio = (p1 : ResultadoRutina, p2 : ResultadoRutina) => p1.peso > p2.peso
    val pokemon =  Pokemon(nivel = 1, experiencia = 349, especie = Charizard, fuerza = 10, ataques=List(mordida))
    val rutina1 = new Rutina(nombre="Rutina 1", actividades = RealizarUnAtaque(enfocarse)) //no tiene el ataque
    val rutina2 = new Rutina(nombre="Rutina 2", actividades = RealizarUnAtaque(mordida)) //tiene el ataque, le sube experiencia
    val rutina3 = new Rutina(nombre="Rutina 3", actividades = RealizarUnAtaque(explosion)) //no tiene el ataque
    
    val analizador = new AnalizadorRutinas(mayorNivelCriterio, rutina1, rutina2, rutina3)
    assertEquals("Rutina 2" , analizador.mejorRutina(pokemon))
  }
  
  @Test
  def `Obtener mejor rutina que obtenga el menor peso para un pokemon` = { 
  
    val menorPesoCriterio = (r1 : ResultadoRutina, r2 : ResultadoRutina) => r1.pokemon.peso < r2.pokemon.peso
    val pokemon =  Pokemon(genero = Femenino, peso = 100,  especie = Charizard)
    val rutina1 = new Rutina(nombre="Rutina 1", actividades = FingirIntercambio) 
    val rutina2 = new Rutina(nombre="Rutina 2", actividades = FingirIntercambio, FingirIntercambio)
    val rutina3 = new Rutina(nombre="Rutina 3", actividades = FingirIntercambio, FingirIntercambio, FingirIntercambio) 
    
    val analizador = new AnalizadorRutinas(menorPesoCriterio, rutina1, rutina2, rutina3)
    assertEquals("Rutina 3" , analizador.mejorRutina(pokemon))
  }
  
  @Test
  def `Obtener mejor rutina que obtenga la moyor energia para un pokemon` = { 
  
    val mayorEnergiaCriterio = (r1 : ResultadoRutina, r2 : ResultadoRutina) => r1.pokemon.energia > r2.pokemon.energia
    val pokemon =  Pokemon(genero = Femenino, energia = 0,  especie = Charizard)
    val rutina1 = new Rutina(nombre="Rutina 1", actividades = FingirIntercambio, UsarPocion) 
    val rutina2 = new Rutina(nombre="Rutina 2", actividades = FingirIntercambio, FingirIntercambio)
    val rutina3 = new Rutina(nombre="Rutina 3", actividades = FingirIntercambio, FingirIntercambio, FingirIntercambio) 
    
    val analizador = new AnalizadorRutinas(mayorEnergiaCriterio, rutina1, rutina2, rutina3)
    assertEquals("Rutina 1" , analizador.mejorRutina(pokemon))
  }
  
}
