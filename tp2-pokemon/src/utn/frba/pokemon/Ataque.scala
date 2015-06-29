package utn.frba.pokemon
import scala.util.{Try, Success, Failure}

//Los Ataques son un tipo de Actividad
case class Ataque(val tipo : Tipo = Normal, val puntosDeAtaque : Int = 1, val maximoInicialPA : Int = 10, val efecto: (Pokemon => Try[Pokemon])) extends Actividad {
  
  def aplicar(pokemon : Pokemon) : Try[Pokemon] = {
    var experiencia = 0
    
    pokemon match {
      case _ if this.puntosDeAtaque == 0 => return Try(throw PuntosDeAtaqueInsuficientesExceltion ("Pokemon no puede realizar ataque: No tiene suficientes puntos de ataque para atacar"))
      case _ if !pokemon.ataques.contains(this) => return Try(throw UnknownAttackException("Pokemon no puede realizar ataque: No conoce el ataque %s".format(this.getClass.toString())))
      case _ if tipo == Dragon => experiencia = 80
      case _ if pokemon.esTipoPrincipal(tipo) => experiencia = 50
      case _ if pokemon.esTipoSecundario(tipo) && pokemon.esHembra => experiencia = 40
      case _ if pokemon.esTipoSecundario(tipo) && pokemon.esMacho => experiencia = 20
    }

    val a1 = pokemon.ataques.find((a: Ataque) => a == this).get.bajarPA
    val listaSinAtaqueActual = pokemon.ataques.filter { a: Ataque => a != this }
    val pok = pokemon.copy(ataques = listaSinAtaqueActual :+ a1)
    
    efecto(pok.cambiarExperiencia(experiencia))
  }
  
   def bajarPA: Ataque = this match {
    case Ataque(t,pa,m,e) => Ataque(t,pa-1,m,e)
  } 
}

