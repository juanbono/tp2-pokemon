package utn.frba.pokemon

/*
 ******* Actividades que faltan *******
 case Descansar =>
 case FingirIntercambio =>
 case AprenderAtaque(ataque) => 
 case UsarPiedra(piedra) => 
      
  */

object Simulador {
  def entrenar(p: Pokemon, rutina: Actividad*): Estado = rutina.foldLeft(EstadoNormal(p): Estado) { (actividadAnterior, actividadActual) =>
    val estadoAnterior = actividadAnterior
    if (rutina.isEmpty) EstadoNormal(p) // si no hay ninguna actividad , retorno el pokemon en estado normal.

    actividadActual match {

      case RealizarUnAtaque(ataque @ _) => ataque match {
        case _ if (ataque.tipo == Dragon)                         => ataque.ejecutar(estadoAnterior, 80)
        case _ if p.esTipoPrincipal(ataque.tipo)                  => ataque.ejecutar(estadoAnterior, 50)
        case _ if (p.esTipoSecundario(ataque.tipo) && p.esMacho)  => ataque.ejecutar(estadoAnterior, 20)
        case _ if (p.esTipoSecundario(ataque.tipo) && p.esHembra) => ataque.ejecutar(estadoAnterior, 40)
        // casos de fallo
        case _ if !(p.ataques.contains(ataque))                   => KO(p, "no conoce el ataque")
        case _ if (ataque.puntosDeAtaque < 0)                     => KO(p, "no tiene suficientes PA")
      }

      case ComerZinc => estadoAnterior.map { p =>
        val ataquesModificados = p.ataques.map {
          _ match {
            case AtaqueConEstado(a, b, maximoPA, c) => AtaqueConEstado(a, b, maximoPA + 2, c)
            case AtaqueSinEstado(a, b, maximoPA, c) => AtaqueSinEstado(a, b, maximoPA + 2, c)
          }
        }
        estadoAnterior.pokemon.copy(ataques = ataquesModificados)
      }

      case LevantarPesas(kilos) => estadoAnterior match {
        case _ if p.algunTipoEs(Pelea) => estadoAnterior.map { _ => estadoAnterior.pokemon.subirExperiencia(2 * kilos)}
        case _ if (p.fuerza * 10 < kilos) => Paralizado(estadoAnterior.pokemon)
        case _ if estadoAnterior.isInstanceOf[Paralizado] => KO(estadoAnterior.pokemon, "El pokemon quedo KO")

      }

      case Nadar(minutos) => estadoAnterior match {
        case _ if p.esTipoPrincipal(Agua) => estadoAnterior.map { _ =>
          val p1 = estadoAnterior.pokemon.subirVelocidad(minutos % 60);
          val p2 = p1.subirExperiencia(200);
          p2.bajarEnergia(minutos) // fijarse como componer estas acciones
        }
        case _ if ((Agua.mataA(p.tipoPrincipal)) || (Agua.mataA(p.tipoSecundario))) => KO(estadoAnterior.pokemon, "El pokemon perdia contra el agua")
        case _ => estadoAnterior.map { _ =>
          val p1 = estadoAnterior.pokemon.subirExperiencia(200);
          p1.bajarEnergia(minutos)
        }
      }

      case UsarAntidoto => estadoAnterior match {
        case Envenenado(_) => EstadoNormal(estadoAnterior.pokemon)
        case _             => estadoAnterior
      }

      case UsarEther => estadoAnterior match {
        case KO(_, _) => estadoAnterior
        case _        => EstadoNormal(estadoAnterior.pokemon)

      }

      case otra =>
        estadoAnterior.map { p =>
          val pSiguiente = otra match {
            case UsarPocion  => p.subirEnergia(50)
            case ComerHierro => p.subirFuerza(5)
            case ComerCalcio => p.subirVelocidad(5)
          }
          pSiguiente
        }
    }
  }
}
