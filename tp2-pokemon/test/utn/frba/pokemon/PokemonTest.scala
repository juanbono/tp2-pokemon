package utn.frba.pokemon

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.assertFalse
import org.junit.Test
import org.junit.Ignore

// En esta clase se testea el modelo de pokemon y actividades individualmente

class EvolucionTest {

  
  @Test
    def `Pokemon con valores invalidos` = {
      val poke =  Pokemon(nivel=0, especie = Charizard, ataques=List(new MordidaAtaque))
      
      assertFalse(poke.esPokemonValido)
  }
      
  @Test
  def `Pokemon gana 50 de experiencia y sube de nivel.` = {
    
    //Tipo de ataque es igual al tipo princial del pokemon.
    var ataque = new MordidaAtaque(puntosDeAtaque = 1, maximoInicialPA=10)
    var pokemon = new Pokemon(nivel=1, experiencia=300, especie = Charizard, ataques=List(ataque))
    var actividad = new RealizarUnAtaque(ataque)
    
    val resultado = actividad.ejecutar(pokemon)    
    
    assertEquals(2, resultado.pokemon.nivel)
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
      especie = Nidorina
    )
    
		pokemon = Simulador.entrenar(pokemon, UsarPiedra(PiedraLunar())).fold(null)(p => p)

		assertEquals(pokemon.nivel, 25)
		assertEquals(pokemon.especie, Nidoqueen)
	}

	@Test
	def `Evolucionar por usar piedra del mismo tipo` {
		var pokemon = Pokemon(
        nivel = 25,
        especie = Poliwhirl
    )
    
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
}