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
        case _ if (ataque.tipo == Dragon) => ataque.ejecutar(estadoAnterior, 80)

        case AtaqueConEstado(tipo, _, _, f) if (tipo == p.especie.tipoPrincipal) => ataque.ejecutar(estadoAnterior, 50)
        case AtaqueConEstado(tipo, _, _, f) if ((tipo == p.especie.tipoSecundario) && (p.genero == Masculino)) => ataque.ejecutar(estadoAnterior, 20)
        case AtaqueConEstado(tipo, _, _, f) if ((tipo == p.especie.tipoSecundario) && (p.genero == Femenino)) => ataque.ejecutar(estadoAnterior, 40)

        case AtaqueSinEstado(tipo, _, _, f) if (tipo == p.especie.tipoPrincipal) => ataque.ejecutar(estadoAnterior, 50)
        case AtaqueSinEstado(tipo, _, _, f) if ((tipo == p.especie.tipoSecundario) && (p.genero == Masculino)) => ataque.ejecutar(estadoAnterior, 20)
        case AtaqueSinEstado(tipo, _, _, f) if ((tipo == p.especie.tipoSecundario) && (p.genero == Femenino)) => ataque.ejecutar(estadoAnterior, 40)

        // casos de fallo
        case _ if !(p.ataques.contains(ataque)) => KO(p, "no conoce el ataque")
        case _ if (ataque.puntosDeAtaque < 0) => KO(p, "no tiene suficientes PA")
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
        case _ if ((p.especie.tipoPrincipal == Pelea) || (p.especie.tipoSecundario == Pelea)) => estadoAnterior.map { _ => estadoAnterior.pokemon.copy(experiencia = estadoAnterior.pokemon.experiencia + 2 * kilos) }
        case _ if (p.fuerza * 10 < kilos) => Paralizado(estadoAnterior.pokemon)
        case _ if estadoAnterior.isInstanceOf[Paralizado] => KO(estadoAnterior.pokemon, "El pokemon quedo KO")

      }

      case Nadar(minutos) => estadoAnterior match {
        case _ if (p.especie.tipoPrincipal == Agua) => estadoAnterior.map { _ => estadoAnterior.pokemon.copy(velocidad = estadoAnterior.pokemon.velocidad + (minutos % 60), experiencia = estadoAnterior.pokemon.experiencia + 200, energia = estadoAnterior.pokemon.energia - minutos) }
        case _ if ((Agua.mataA.contains(p.especie.tipoPrincipal)) || (Agua.mataA.contains(p.especie.tipoSecundario))) => KO(estadoAnterior.pokemon, "El pokemon perdia contra el agua")
        case _ => estadoAnterior.map { _ => estadoAnterior.pokemon.copy(experiencia = estadoAnterior.pokemon.experiencia + 200, energia = estadoAnterior.pokemon.energia - minutos) }
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
            case UsarPocion  => p.copy(energia = p.energia + 50)
            case ComerHierro => p.copy(fuerza = p.fuerza + 5)
            case ComerCalcio => p.copy(velocidad = p.velocidad + 5)
          }
          pSiguiente
        }
    }
  }
}
