package utn.frba.pokemon

case class Ataque(val tipo : Tipo = Normal, val puntosDeAtaque : Int = 1, val maximoInicialPA : Int = 10, val efecto: (Pokemon => Pokemon) = (p: Pokemon) => p) extends Actividad {
  
  def puedeAtacar(pokemon : Pokemon) : Boolean = this.puntosDeAtaque > 0 && pokemon.ataques.contains(this)
  
  def ejecutar(pokemon : Pokemon) : Pokemon = {
    var experiencia = 0

    if (tipo == Dragon)
      experiencia = 80
    else if (pokemon.esTipoPrincipal(tipo))
      experiencia = 50
    else if (pokemon.esTipoSecundario(tipo) && pokemon.esHembra)
      experiencia = 40
    else if (pokemon.esTipoSecundario(tipo) && pokemon.esMacho)
      experiencia = 20
     
    efecto(pokemon.copy(experiencia = pokemon.experiencia + experiencia))

  }
}

/* Aca se definen los distintos tipos de ataques disponibles.
 * El efecto implica tanto el comportamiendo del ataque como el concepto de efecto colateral definido en el tp.
 */
class AtaqueDefault(tipo : Tipo = Normal, puntosDeAtaque : Int = 1, maximoInicialPA : Int= 5, 
     override val efecto : (Pokemon => Pokemon) = (p: Pokemon) => p.subirEnergia(1)) extends Ataque

class MordidaAtaque(override val tipo : Tipo = Fuego, override val puntosDeAtaque : Int = 1, override val  maximoInicialPA : Int= 30,
      override val efecto : (Pokemon => Pokemon) = (p: Pokemon) => p.subirFuerza(1)) extends Ataque

class ReporsarAtaque (override val tipo : Tipo = Fuego, override val puntosDeAtaque : Int = 1, override val maximoInicialPA : Int = 2,
      override val efecto : (Pokemon => Pokemon) = (p: Pokemon) => p.copy(energia = p.energiaMaxima)) extends Ataque

class EnfocarseAtaque (override val tipo : Tipo = Fuego,  override val puntosDeAtaque  : Int= 1,  override val maximoInicialPA : Int  = 2,
     override val efecto : (Pokemon => Pokemon) = (p: Pokemon) => p.subirVelocidad(1)) extends Ataque 

class EndurecerseAtaque (override val tipo : Tipo = Fuego,  override val puntosDeAtaque : Int  = 1, override val  maximoInicialPA : Int = 2,
     override val efecto : (Pokemon => Pokemon) = (p: Pokemon) => p.subirEnergia(5)) extends Ataque
    
class ChorroDeAguaDelRiachueloAtaque (override val tipo : Tipo = Agua, override val  puntosDeAtaque : Int = 1, override val  maximoInicialPA : Int = 10,
     override val efecto : (Pokemon => Pokemon) = (p: Pokemon) => p.subirFuerza(1)) extends Ataque 

class ExplosionAtaque (override val tipo : Tipo = Fuego,  override val puntosDeAtaque : Int  = 1,  override val maximoInicialPA : Int  = 20,
    efecto : (Pokemon => Pokemon) = (p: Pokemon) => p) extends Ataque 