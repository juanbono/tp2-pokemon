package utn.frba.pokemon

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.Ignore

class GimnasioPokemonTest {

  @Test(expected=classOf[IllegalArgumentException])
  def `Pokemon con valores invalidos` = {
    val poke =  Pokemon(nivel=0, especie = Charizard)
  }

  @Test
  def `Pokemon gana experiencia` = {
    
    var ataque = new MordidaAtaque(puntosDeAtaque = 1, maximoInicialPA=10)
    var pokemon = new Pokemon(nivel=1, experiencia=0, especie = Charizard, ataques=Some(List(ataque)))
    var actividad = new RealizarUnAtaque(ataque)
    
    val resultado = actividad.ejecutar(pokemon)
    val pokemonResultado = resultado.pokemon
    
    assertEquals(50, pokemonResultado.get.experiencia)

  }
  
  
}
