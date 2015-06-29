package utn.frba
import scala.util.{Try, Success, Failure}

package object pokemon {
  
  // *** Definicion de Tipos *** 
  trait Tipo {
  def tiposPeores : List[Tipo]
  def mataA(tipo : Tipo) : Boolean = tiposPeores.contains(tipo)
  }
  
  object Fuego extends Tipo {def tiposPeores = List(Planta, Hielo, Bicho)}
  object Agua extends Tipo {def tiposPeores = List(Fuego, Tierra, Roca)}
  object Planta extends Tipo {def tiposPeores = List(Agua, Tierra, Roca)}
  object Tierra extends Tipo {def tiposPeores = List(Fuego, Electrico, Veneno, Roca)}
  object Hielo extends Tipo {def tiposPeores = List(Planta, Tierra, Volador, Dragon)}
  object Roca extends Tipo {def tiposPeores = List(Fuego, Hielo, Volador, Bicho)}
  object Electrico extends Tipo {def tiposPeores = List(Agua, Volador)}
  object Psiquico extends Tipo {def tiposPeores = List(Pelea, Veneno)}
  object Pelea extends Tipo {def tiposPeores = List(Normal, Hielo, Roca)}
  object Fantasma extends Tipo {def tiposPeores = List(Fantasma, Psiquico)}
  object Volador extends Tipo {def tiposPeores = List(Planta, Pelea, Bicho)}
  object Bicho extends Tipo {def tiposPeores = List(Planta, Psiquico)}
  object Veneno extends Tipo {def tiposPeores : List[Tipo] = List(Planta, Hielo, Bicho)}
  object Dragon extends Tipo {def tiposPeores = List(Dragon)}
  object Normal extends Tipo {def tiposPeores = List()}
  
  // *** Definicion de Estados ***
  abstract class Estado
  case object EstadoNormal extends Estado
  case object EstadoEnvenenado extends Estado
  case class  EstadoKO(msg: String) extends Estado
  case object EstadoParalizado extends Estado
  case class  EstadoDormido(val turnos: Int) extends Estado
  
  // *** Definicion de Ataques
  
 // ***  Aca se definen los distintos tipos de ataques disponibles.
 // El efecto implica tanto el comportamiendo del ataque como el concepto de efecto colateral definido en el tp.

val AtaqueDefault = Ataque(tipo = Normal, puntosDeAtaque  = 1, maximoInicialPA = 5, efecto = (p: Pokemon) => Try(p.cambiarEnergia(1)))
val MordidaAtaque = Ataque(tipo = Fuego, puntosDeAtaque  = 1, maximoInicialPA = 30, efecto = (p: Pokemon) => Try(p.cambiarFuerza(1)))
val ReporsarAtaque = Ataque(tipo = Fuego, puntosDeAtaque  = 1, maximoInicialPA = 2, efecto = (p: Pokemon) => Try(p.copy(energia = p.energiaMaxima).cambiarEstado(EstadoDormido(3))))
val EnfocarseAtaque = Ataque(tipo = Fuego, puntosDeAtaque  = 1, maximoInicialPA = 2, efecto = (p: Pokemon) => Try(p.cambiarVelocidad(1)))
val EndurecerseAtaque = Ataque(tipo = Fuego, puntosDeAtaque  = 1, maximoInicialPA = 2, efecto = (p: Pokemon) => Try(p.cambiarEnergia(5).cambiarEstado(EstadoParalizado)))
val ChorroDeAguaDelRiachueloAtaque = Ataque(tipo = Agua, puntosDeAtaque  = 1, maximoInicialPA = 10, efecto =(p: Pokemon) => Try(p.cambiarFuerza(1).cambiarEstado(EstadoEnvenenado)))
val ExplosionAtaque = Ataque(tipo = Agua, puntosDeAtaque  = 1, maximoInicialPA = 20, efecto =(p: Pokemon) => Try(p.cambiarEstado(EstadoKO("KO por explosion"))))


  class Rutina(val nombre: String, val actividades: Actividad*) {
    def ejecutar(pokemon: Pokemon): Try[Pokemon] = entrenarPokemon(pokemon, actividades: _*)
    
    def detalleEjecucion : String = "Es necesario tener en cuenta que no necesariamente todo Pokémon es capaz de realizar" +
                                    "cualquier Rutina y, en caso de que no pueda, sería importante poder averiguar porqué."
  }
  
  // Ejecuta una secuencia de actividades sobre un pokemon
  def entrenarPokemon(pokemon: Pokemon, rutina: Actividad*): Try[Pokemon] = rutina.foldLeft(Try(pokemon)) { (pokemonAnterior, actividadActual) =>
    pokemonAnterior.flatMap { pokemon => actividadActual.ejecutar(pokemon) }
  }
  
  // Devuelve el nombre de la mejor rutina para un pokemon en base a un criterio.
  def obtenerMejorRutina(pokemon: Pokemon, criterio: (Pokemon, Pokemon) => Boolean, rutinas: Rutina*): String = {
    val rutinasFinalizadas = rutinas.map { rutina => (rutina.nombre, rutina.ejecutar(pokemon)) }.filter { _._2.isSuccess }.map {tupla => (tupla._1, tupla._2.get)}
    val mejorPokemon = rutinasFinalizadas.map {tupla => tupla._2}.sortWith(criterio).headOption
       
    mejorPokemon match {
      case None    => "Ninguna rutina"
      case Some(p) => rutinasFinalizadas.find(_._2 == p).get._1
    }
  }

  
  
  
}