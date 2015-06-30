package utn.frba
import scala.util.{ Try, Success, Failure }

package object pokemon {

  //**************************** Tipos *******************************
  trait Tipo {
    def tiposPeores: List[Tipo]
    def mataA(tipo: Tipo): Boolean = tiposPeores.contains(tipo)
  }

  object Fuego extends Tipo { def tiposPeores = List(Planta, Hielo, Bicho) }
  object Agua extends Tipo { def tiposPeores = List(Fuego, Tierra, Roca) }
  object Planta extends Tipo { def tiposPeores = List(Agua, Tierra, Roca) }
  object Tierra extends Tipo { def tiposPeores = List(Fuego, Electrico, Veneno, Roca) }
  object Hielo extends Tipo { def tiposPeores = List(Planta, Tierra, Volador, Dragon) }
  object Roca extends Tipo { def tiposPeores = List(Fuego, Hielo, Volador, Bicho) }
  object Electrico extends Tipo { def tiposPeores = List(Agua, Volador) }
  object Psiquico extends Tipo { def tiposPeores = List(Pelea, Veneno) }
  object Pelea extends Tipo { def tiposPeores = List(Normal, Hielo, Roca) }
  object Fantasma extends Tipo { def tiposPeores = List(Fantasma, Psiquico) }
  object Volador extends Tipo { def tiposPeores = List(Planta, Pelea, Bicho) }
  object Bicho extends Tipo { def tiposPeores = List(Planta, Psiquico) }
  object Veneno extends Tipo { def tiposPeores: List[Tipo] = List(Planta, Hielo, Bicho) }
  object Dragon extends Tipo { def tiposPeores = List(Dragon) }
  object Normal extends Tipo { def tiposPeores = List() }
  
  //**************************** Estados ***********************************
  abstract class Estado
  case object Envenenado extends Estado
  case class KO(msg: String) extends Estado
  case object Paralizado extends Estado
  case class Dormido(turnos: Int) extends Estado
  //**************************** Actividades ***************************

  type Actividad = Pokemon => Pokemon

  val usarPocion: Actividad = p => p.cambiarEnergia(50)

  val usarAntidoto: Actividad = p => if (p.estado.get == Envenenado) p.cambiarEstado(None) else p

  val usarEther: Actividad = p => if (!p.estaKO) p.cambiarEstado(None) else p

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
    case _ if p.algunTipoEs(Fantasma) => throw InvalidPokemonTypeException("Los pokemon de tipo fantasma no pueden levantar pesas.")
    case _ if kg / p.fuerza > 10      => p.cambiarEstado(Some(Paralizado))
    case _ if p.algunTipoEs(Pelea)    => p.subirExperiencia(2 * kg)
    case _                            => p.subirExperiencia(kg)
  }

  val nadar: Int => Actividad = (min: Int) => (p: Pokemon) => p match {
    case _ if p.especie.debilContra(Agua) => p.cambiarEstado(Some(KO("La especie de este pokemon era debil al agua.")))
    case _                                => p.subirExperiencia(200).cambiarEnergia(-min).cambiarVelocidad((if (p.esTipoPrincipal(Agua)) min % 60 else 0))
  }

  val usarPiedra: PiedraEvolutiva => Actividad = (piedra: PiedraEvolutiva) => (p: Pokemon) => p match {
    case _ if piedra.tipo.mataA(p.tipoPrincipal) || piedra.tipo.mataA(p.tipoSecundario.getOrElse(null)) => p.cambiarEstado(Some(Envenenado))
    case _ => p.especie.condicionEvolucion.fold(p)(_.usarPiedra(p, piedra))
  }

  val aprenderAtaque: Ataque => Actividad = (a: Ataque) => (p: Pokemon) => p match {
    case _ if p.algunTipoEs(a.tipo) || a.tipo == Normal => p.copy(ataques = a.copy(puntosDeAtaque = a.maximoInicialPA) :: p.ataques)
    case _ => p.cambiarEstado(Some(KO("Pokemon se lastimo trantando de aprender ataque")))
  }

  val realizarAtaque: Ataque => Actividad = (a: Ataque) => (p: Pokemon) => a.aplicar(p)

  //**************************** Simulador *******************************
  // quiza puede ir aca el simulador
}


