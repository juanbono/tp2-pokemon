package utn.frba.pokemon

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.Ignore
class EvolucionTest {

	@Test
	def `Evolucionar por subir de nivel` {
		var pokemon = Pokemon(
				nivel = 15,
				especie = Charmander,
				experiencia = 999999
				)

				pokemon = pokemon.subirNivel

				assertEquals(pokemon.nivel, 16)
				assertEquals(pokemon.especie, Charmeleon)
	}

	@Test
	def `Evolucionar por usar piedra lunar` {
		var pokemon = Pokemon(
				nivel = 25,
				especie = Nidorina
				)

				var piedra = PiedraEvolutiva(Lunar)

				pokemon = pokemon.usarPiedra(piedra)

				assertEquals(pokemon.nivel, 25)
				assertEquals(pokemon.especie, Nidoqueen)
	}

	@Test
	def `Evolucionar por usar piedra del mismo tipo` {
		var pokemon = Pokemon(
				nivel = 25,
				especie = Poliwhirl
				)

				var piedra = PiedraEvolutiva(Agua)

				pokemon = pokemon.usarPiedra(piedra)

				assertEquals(pokemon.nivel, 25)
				assertEquals(pokemon.especie, Poliwrath)
	}

	@Test
	def `Usar piedra envenenadora` {
		var pokemon = Pokemon(
				nivel = 25,
				especie = Poliwhirl
				)

				var piedra = PiedraEvolutiva(Electrico)

				pokemon = pokemon.usarPiedra(piedra)

				assertEquals(pokemon.estado, EstadoEnvenenado)
	}
}