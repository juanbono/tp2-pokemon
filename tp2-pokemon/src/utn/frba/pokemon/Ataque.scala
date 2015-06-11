package utn.frba.pokemon

//Los Ataques son un tipo de Actividad
case class Ataque(val tipo : Tipo = Normal, val puntosDeAtaque : Int = 1, val maximoInicialPA : Int = 10, val efecto: (Pokemon => Estado) = (p: Pokemon) => EstadoNormal(p)) extends Actividad {
  
  def aplicar(pokemon : Pokemon) : Estado = {
    var experiencia = 0
    
    pokemon match {
      case _ if this.puntosDeAtaque == 0 => return EstadoActividadNoEjecutada(pokemon, "Pokemon no puede realizar ataque: No tiene suficientes puntos de ataque para atacar")
      case _ if !pokemon.ataques.contains(this) => return EstadoActividadNoEjecutada(pokemon, "Pokemon no puede realizar ataque: No conoce el ataque %s".format(this.getClass.toString()))
      case _ if tipo == Dragon => experiencia = 80
      case _ if pokemon.esTipoPrincipal(tipo) => experiencia = 50
      case _ if pokemon.esTipoSecundario(tipo) && pokemon.esHembra => experiencia = 40
      case _ if pokemon.esTipoSecundario(tipo) && pokemon.esMacho => experiencia = 20
    }
    
    efecto(pokemon.copy(experiencia = pokemon.experiencia + experiencia).subirNivel)
  }
}

/* Aca se definen los distintos tipos de ataques disponibles.
 * El efecto implica tanto el comportamiendo del ataque como el concepto de efecto colateral definido en el tp.
 */
class AtaqueDefault(tipo : Tipo = Normal, puntosDeAtaque : Int = 1, maximoInicialPA : Int= 5, 
     override val efecto : (Pokemon => Estado) = (p: Pokemon) => EstadoNormal(p.subirEnergia(1))) extends Ataque

class MordidaAtaque(override val tipo : Tipo = Fuego, override val puntosDeAtaque : Int = 1, override val  maximoInicialPA : Int= 30,
      override val efecto : (Pokemon => Estado) = (p: Pokemon) => EstadoNormal(p.subirFuerza(1))) extends Ataque

class ReporsarAtaque (override val tipo : Tipo = Fuego, override val puntosDeAtaque : Int = 1, override val maximoInicialPA : Int = 2,
      override val efecto : (Pokemon => Estado) = (p: Pokemon) => EstadoDormido(p.copy(energia = p.energiaMaxima))) extends Ataque

class EnfocarseAtaque (override val tipo : Tipo = Fuego,  override val puntosDeAtaque  : Int= 1,  override val maximoInicialPA : Int  = 2,
     override val efecto : (Pokemon => Estado) = (p: Pokemon) => EstadoNormal(p.subirVelocidad(1))) extends Ataque 

class EndurecerseAtaque (override val tipo : Tipo = Fuego,  override val puntosDeAtaque : Int  = 1, override val  maximoInicialPA : Int = 2,
     override val efecto : (Pokemon => Estado) = (p: Pokemon) => EstadoParalizado(p.subirEnergia(5))) extends Ataque
    
class ChorroDeAguaDelRiachueloAtaque (override val tipo : Tipo = Agua, override val  puntosDeAtaque : Int = 1, override val  maximoInicialPA : Int = 10,
     override val efecto : (Pokemon => Estado) = (p: Pokemon) => EstadoEnvenenado(p.subirFuerza(1))) extends Ataque 

class ExplosionAtaque (override val tipo : Tipo = Fuego,  override val puntosDeAtaque : Int  = 1,  override val maximoInicialPA : Int  = 20,
     override val efecto : (Pokemon => Estado) = (p: Pokemon) => EstadoKO(p, "KO por explosion")) extends Ataque 