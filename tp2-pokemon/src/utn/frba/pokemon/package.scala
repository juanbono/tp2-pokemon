package utn.frba

import utn.frba.pokemon._
// Averiguar si es util/copado poner todo en un package object.
package object simulador {

  //**************************** Tipos *******************************
  case class Tipo(val lst: List[Tipo]) {
    def mataA(tipo: Tipo): Boolean = lst.contains(tipo)
  }

  lazy val fuego: Tipo = Tipo(List(fuego))
  lazy val agua: Tipo = Tipo(List(fuego, tierra, roca))
  lazy val planta: Tipo = Tipo(List(agua, tierra, roca))
  lazy val tierra: Tipo = Tipo(List(fuego, electrico, veneno, roca))
  lazy val hielo: Tipo = Tipo(List(planta, tierra, volador, dragon))
  lazy val roca: Tipo = Tipo(List(fuego, hielo, volador, bicho))
  lazy val electrico: Tipo = Tipo(List(agua, volador))
  lazy val psiquico: Tipo = Tipo(List(pelea, veneno))
  lazy val pelea: Tipo = Tipo(List(normal, hielo, roca))
  lazy val fantasma: Tipo = Tipo(List(psiquico, fantasma))
  lazy val volador: Tipo = Tipo(List(planta, pelea, bicho))
  lazy val bicho: Tipo = Tipo(List(planta, psiquico))
  lazy val veneno: Tipo = Tipo(List(planta))
  lazy val dragon: Tipo = Tipo(List(dragon))
  lazy val normal: Tipo = Tipo(List())

  //**************************** Ataques *******************************

  //**************************** Especie *******************************
  case class Especie(
    val id: Int = 0, // MissingNo.
    val resistenciaEvolutiva: Int = 10,
    val pesoMaximo: Double = 0,
    val energiaMaximaInc: Int = 0,
    val pesoInc: Double = 0,
    val fuerzaInc: Int = 0,
    val velocidadInc: Int = 0,
    val tipoPrincipal: Tipo,
    val tipoSecundario: Option[Tipo] = None,
    val evolucion: Option[Especie] = None,
    val condicionEvolucion: Option[CondicionEvolucion] = None)

  //**************************** Pokemon *******************************

  //**************************** Actividades ***************************

  type Actividad = Pokemon => Pokemon

  val tomarPocion: Actividad = p => p.cambiarEnergia(50)
  val tomarAntidoto: Actividad = p => if (p.estado.get == Envenenado) p.cambiarEstado(None) else p
  val usarEther: Actividad = p => if (p.estado.get != KO) p.cambiarEstado(None) else p
  val comerHierro: Actividad = p => p.cambiarFuerza(5)
  val comerCalcio: Actividad = p => p.cambiarVelocidad(5)
  val comerZinc: Actividad = p => p.copy(ataques = p.ataques.map { a => a.copy(maximoInicialPA = a.maximoInicialPA + 2) })
  val descansar: Actividad = { p =>
    val pokemon = p.estado match {
      case None if (p.energia < p.energiaMaxima / 2) => p.cambiarEstado(Some(Dormido(0))).copy(ataques = p.ataques.map { a => a.recuperarTodosLosPA })
      case _                                         => p.copy(ataques = p.ataques.map { a => a.recuperarTodosLosPA })
    }
    pokemon
  }
  val fingirIntercambio: Actividad = { p =>
    val nuevoPokemon = pokemon.especie.condicionEvolucion.fold(pokemon)(_.intercambiar(pokemon))
    // hay qe pasar pokemon y especie al package parece xD (nota mental: fijarme si es realmente necesario) 
    if (nuevoPokemon.esHembra)
      nuevoPokemon.bajarPeso(10)
    else
      nuevoPokemon.subirPeso(1)
  }
  //**************************** Simulador *******************************
}


