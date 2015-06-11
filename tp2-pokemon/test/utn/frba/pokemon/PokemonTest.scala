package utn.frba.pokemon

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.Ignore
class EvolucionTest {
  
  
  @Test(expected=classOf[IllegalArgumentException])
    def `Pokemon con valores invalidos` = {
      val poke =  Pokemon(nivel=0, especie = Charizard, ataques=List(new MordidaAtaque))
  }
      
  @Test
  def `Pokemon gana 50 de experiencia y sube de nivel.` = {
    
    //Tipo de ataque es igual al tipo princial del pokemon.
    var ataque = new MordidaAtaque(puntosDeAtaque = 1, maximoInicialPA=10)
    var pokemon = new Pokemon(nivel=1, experiencia=300, especie = Charizard, ataques=List(ataque))
    var actividad = new RealizarUnAtaque(ataque)
    
    pokemon = actividad.ejecutar(pokemon)    
    
    assertEquals(2, pokemon.nivel)
    assertEquals(350, pokemon.experiencia)
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
		var pokemon = Pokemon(nivel = 25, especie = Nidorina)

				var piedra = new PiedraLunar

				pokemon = pokemon.usarPiedra(piedra)

				assertEquals(pokemon.nivel, 25)
				assertEquals(pokemon.especie, Nidoqueen)
	}

	@Test
	def `Evolucionar por usar piedra del mismo tipo` {
		var pokemon = Pokemon(nivel = 25,	especie = Poliwhirl)

				var piedra = new PiedraEvolutivaComun(Agua)

				pokemon = pokemon.usarPiedra(piedra)

				assertEquals(pokemon.nivel, 25)
				assertEquals(pokemon.especie, Poliwrath)
	}

  
  //TODO  arreglar este test, ya no se devuelven estados de esta forma, hay que usar el simulador
	@Test
	def `Usar piedra envenenadora` {
		var pokemon = Pokemon(nivel = 25, especie = Poliwhirl)

				var piedra = PiedraEvolutivaComun(Electrico)

				pokemon = pokemon.usarPiedra(piedra)

				//assertEquals(pokemon.estado, EstadoEnvenenado)
	}
}