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
  val ExplosionAtaque = Ataque(tipo = Fuego, puntosDeAtaque  = 1, maximoInicialPA = 20, efecto =(p: Pokemon) => Try(p.cambiarEstado(EstadoKO("KO por explosion"))))

  type Rutina = (String, List[Actividad])
  def actividades(r: Rutina) = r._2
  def nombre(r: Rutina) = r._1
  
  // Ejecuta una secuencia de actividades sobre un pokemon
  def realizarRutina(pokemon: Pokemon, rutina: Rutina): Try[Pokemon] = actividades(rutina).foldLeft(Try(pokemon)) { (pokemonAnterior, actividadActual) =>
    pokemonAnterior.flatMap { pokemon => actividadActual(pokemon) }
  }
  
  // Devuelve el nombre de la mejor rutina para un pokemon en base a un criterio.
  def analizarRutinas(pokemon: Pokemon, criterio: (Pokemon, Pokemon) => Boolean, rutinas: Rutina*): String = {
    val rutinasFinalizadas = rutinas.map { rutina => (nombre(rutina), realizarRutina(pokemon, rutina)) }.filter { _._2.isSuccess }.map {tupla => (tupla._1, tupla._2.get)}
    val mejorPokemon = rutinasFinalizadas.map {tupla => tupla._2}.sortWith(criterio).headOption
       
    mejorPokemon match {
      case None    => "Ninguna rutina"
      case Some(p) => rutinasFinalizadas.find(_._2 == p).get._1
    }
  }
  
  // *** Actividades ****
  
  // Funcion que ejecuta las actividades. Recibe una funcion (la actividad en si) y un Pokemon y devuelve un Try[Pokemon]
  // Independiente de como esten implementadas las actividades (clases o funciones)
  val ejecutarActividad : (Pokemon => Try[Pokemon]) => Pokemon => Try[Pokemon] = { actividad => pokemon =>
    pokemon.estado match {
       case EstadoKO(_) => Try(throw KOException("El pokemon no puede realizar la actividad porque esta KO"))
       case EstadoDormido(1) => Try(pokemon.cambiarEstado(EstadoNormal))
       case EstadoDormido(x) => Try(pokemon.cambiarEstado(EstadoDormido(x - 1)))
       case _ => actividad(pokemon)
     }
  }
  
  
  // Actividades definidas como funciones. Esto iria en el bonus
  // Para que funcione comentar la clase Actividad entera
  
//  type Actividad = Pokemon => Try[Pokemon]
//  
//  val RealizarUnAtaque : Ataque => Actividad = ataque => p => { ejecutarActividad((p : Pokemon) => (ataque(p)))(p) } 
//  
//  val LevantarPesas : Int => Actividad = kilos => p => {ejecutarActividad((pokemon : Pokemon) => (
//    pokemon match {
//      case _ if pokemon.estado == EstadoParalizado => Try(pokemon.cambiarEstado(EstadoKO("Pokemon paralizado intento levantar pesas"))) 
//      case _ if pokemon.algunTipoEs(Fantasma) => Try(throw InvalidPokemonTypeException("Los pokemon de tipo fantasma no pueden levantar pesas."))
//      case _ if kilos / pokemon.fuerza > 10 => Try(pokemon.cambiarEstado(EstadoParalizado))
//      case _ if pokemon.algunTipoEs(Pelea) => Try(pokemon.cambiarExperiencia(kilos * 2))
//      case _ => Try(pokemon.cambiarExperiencia(kilos))
//    }))(p)}
//  
//  val Nadar : Int => Actividad = minutos => p => {ejecutarActividad((pokemon : Pokemon) => (
//    pokemon match {
//      case _ if (Agua.mataA(pokemon.tipoPrincipal) || Agua.mataA(pokemon.tipoSecundario.getOrElse(Agua))) =>  Try(pokemon.cambiarEstado(EstadoKO("El pokemon pierde contra el agua")))
//      case _ => Try(pokemon.cambiarExperiencia(200).cambiarEnergia(-minutos).cambiarVelocidad(if (pokemon.tipoPrincipal == Agua) minutos % 60 else 0))
//    }))(p)}
//
//  val AprenderAtaque : Ataque => Actividad = ataque => p => {ejecutarActividad((pokemon : Pokemon) => (
//    pokemon match {
//      case _ if pokemon.algunTipoEs(ataque.tipo) || ataque.tipo == Normal => Try(pokemon.copy(ataques = ataque.copy(puntosDeAtaque = ataque.maximoInicialPA) :: pokemon.ataques))
//      case _ => Try(pokemon.cambiarEstado(EstadoKO("Pokemon se lastimo trantando de aprender ataque")))
//    }))(p)}
//  
//  val UsarPiedra : PiedraEvolutiva => Actividad = piedra => p => {ejecutarActividad((pokemon : Pokemon) => (
//    pokemon match {
//       case _ if piedra.tipo.mataA(pokemon.tipoPrincipal) || piedra.tipo.mataA(pokemon.tipoSecundario.getOrElse(null)) =>  Try(pokemon.cambiarEstado(EstadoEnvenenado))
//       case _ => Try(pokemon.especie.condicionEvolucion.fold(pokemon)(_.evolucionar(pokemon, piedra)))
//    }))(p)}
//  
//  val UsarPocion : Actividad = p => {ejecutarActividad((p : Pokemon) => Try(p.cambiarEnergia(50)))(p)}
//
//  val UsarAntidoto : Actividad = p => {ejecutarActividad((pokemon : Pokemon) => (
//     pokemon.estado match {
//       case EstadoEnvenenado => Try(pokemon.cambiarEstado(EstadoNormal))
//       case _ => Try(pokemon)
//     }))(p)}
//  
//  val UsarEther : Actividad = p => {ejecutarActividad((pokemon : Pokemon) => (
//    pokemon.estado match {
//     case EstadoKO(_) => Try(pokemon)
//     case _ => Try(pokemon.cambiarEstado(EstadoNormal))
//   }))(p)}
//  
//  val ComerHierro : Actividad = p => {ejecutarActividad((p : Pokemon) => Try(p.cambiarFuerza(5)))(p)}
//  
//  val ComerCalcio : Actividad = p => {ejecutarActividad((p : Pokemon) => Try(p.cambiarVelocidad(5)))(p)}
//  
//  val ComerZinc : Actividad = p => {ejecutarActividad((pokemon : Pokemon) => {
//    val ataquesModificados = pokemon.ataques.map (ataque => ataque.copy(maximoInicialPA = ataque.maximoInicialPA + 2))
//    Try(pokemon.copy(ataques = ataquesModificados))
//  })(p)}
//
//  val Descansar : Actividad = p => {ejecutarActividad((pokemon : Pokemon) => {
//   val ataquesModificados = pokemon.ataques.map (ataque => ataque.copy(puntosDeAtaque = ataque.maximoInicialPA))
//   val pokemonResultado = pokemon.copy(ataques = ataquesModificados)
//   pokemon.estado match {
//     case EstadoNormal => if (pokemon.energia < pokemon.energiaMaxima / 2) Try(pokemonResultado.cambiarEstado(EstadoDormido(3))) else Try(pokemonResultado)
//     case _            => Try(pokemonResultado)
//   }})(p)}
//  
//  val FingirIntercambio : Actividad = p => {ejecutarActividad((pokemon : Pokemon) => {
//    val nuevoPokemon = pokemon.especie.condicionEvolucion.fold(pokemon)(_.evolucionar(pokemon))
//    if (nuevoPokemon.esHembra)
//      Try(nuevoPokemon.cambiarPeso(-10))
//    else
//      Try(nuevoPokemon.cambiarPeso(1))
//    })(p)}
}