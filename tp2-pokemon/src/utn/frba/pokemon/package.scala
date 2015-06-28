package utn.frba
import scala.util.{ Try, Success, Failure }

// Averiguar si es util/copado poner todo en un package object.
package object pokemon {

  //**************************** Tipos *******************************
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
  // quiza pueden ir aca los ataques
  //**************************** Actividades ***************************

  type Actividad = Pokemon => Pokemon

  val usarPocion: Actividad = p => p.cambiarEnergia(50)
  
  val usarAntidoto: Actividad = p => if (p.estado.get == Envenenado) p.cambiarEstado(None) else p
  
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
    val nuevoPokemon = p.especie.condicionEvolucion.fold(p)(_.intercambiar(p))
    if (nuevoPokemon.esHembra) nuevoPokemon.cambiarPeso(-10) else nuevoPokemon.cambiarPeso(1)
  }
  
  val levantarPesas: Int => Actividad = (kg: Int) => (p: Pokemon) => p match {
    case _ if p.algunTipoEs(fantasma) => throw InvalidPokemonTypeException("Los pokemon de tipo fantasma no pueden levantar pesas.")
    case _ if kg / p.fuerza > 10      => p.cambiarEstado(Some(Paralizado))
    case _ if p.algunTipoEs(Pelea)    => p.subirExperiencia(2 * kg)
    case _                            => p.subirExperiencia(kg)
  }
  
  val nadar: Int => Actividad = (min: Int) => (p: Pokemon) => p match {
    case _ if p.especie.debilContra(agua) => p.cambiarEstado(Some(KO("La especie de este pokemon era debil al agua.")))
    case _                                => p.subirExperiencia(200).cambiarEnergia(-min).cambiarVelocidad((if (p.esTipoPrincipal(agua)) min % 60 else 0))
  }
  
  val usarPiedra: PiedraEvolutiva => Actividad = (piedra: PiedraEvolutiva) => (p: Pokemon) => p match {
    case _ if piedra.tipo.mataA(p.tipoPrincipal) || piedra.tipo.mataA(p.tipoSecundario.getOrElse(null)) => p.cambiarEstado(Some(Envenenado))
    case _ => p.especie.condicionEvolucion.fold(p)(_.usarPiedra(p, piedra))
  }
  
  val aprenderAtaque: Ataque => Actividad = (a: Ataque) => (p: Pokemon) => p match {
    case _ if p.algunTipoEs(a.tipo) || a.tipo == normal => p.copy(ataques = a.copy(puntosDeAtaque = a.maximoInicialPA) :: p.ataques)
    case _ => p.cambiarEstado(Some(KO("Pokemon se lastimo trantando de aprender ataque")))
  }
  
 val realizarAtaque: Ataque => Actividad = (a: Ataque) => (p: Pokemon) => a.aplicar(p)
 
  //**************************** Simulador *******************************
  // quiza puede ir aca el simulador
}


