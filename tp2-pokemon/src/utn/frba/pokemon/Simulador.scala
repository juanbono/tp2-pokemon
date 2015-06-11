package utn.frba.pokemon

object Simulador {
  def entrenar(p: Pokemon, rutina: Actividad*): Estado = rutina.foldLeft(EstadoNormal(p): Estado) { (estadoAnterior, actividadActual) =>

    actividadActual match {

      case RealizarUnAtaque(ataque) => ataque match {
        case _ if !ataque.puedeAtacar(estadoAnterior.pokemon) => estadoAnterior.flatMap { pokemon => EstadoActividadNoEjecutada(pokemon, "No pudo ejecutar el ataque")} //NUEVO ESTADO CUANDO NO PUEDE REALIZAR UNA ACTIVIDAD?
        case _ : MordidaAtaque   => estadoAnterior.map { pokemon => actividadActual.ejecutar(pokemon)}
        case _ : ReporsarAtaque  => estadoAnterior.flatMap { pokemon => EstadoDormido(actividadActual.ejecutar(pokemon))}
        case _ : EnfocarseAtaque => estadoAnterior.map { pokemon => actividadActual.ejecutar(pokemon)}
        case _ : EndurecerseAtaque => estadoAnterior.flatMap { pokemon => EstadoParalizado(actividadActual.ejecutar(pokemon))}
        case _ : ChorroDeAguaDelRiachueloAtaque => estadoAnterior.flatMap { pokemon => EstadoEnvenenado(actividadActual.ejecutar(pokemon))}
        case _ : ExplosionAtaque =>  estadoAnterior.flatMap { pokemon => EstadoKO(actividadActual.ejecutar(pokemon), "pokemon KO por explosion")}        
      }

      case LevantarPesas(kilos) => estadoAnterior match {
        case EstadoParalizado(_) => estadoAnterior.flatMap { pokemon => EstadoKO(pokemon, "Pokemon paralizado intento levantar pesas") }
        case _ if kilos / estadoAnterior.pokemon.fuerza > 10 => estadoAnterior.flatMap { pokemon => EstadoParalizado(pokemon)}
        case _ if estadoAnterior.pokemon.algunTipoEs(Fantasma) => estadoAnterior.map { pokemon => pokemon } //NUEVO ESTADO CUANDO NO PUEDE REALIZAR UNA ACTIVIDAD?
        case _ => estadoAnterior.map { pokemon => actividadActual.ejecutar(pokemon) }
      }

      case Nadar(minutos) => estadoAnterior match {
        case _ if ((Agua.mataA(estadoAnterior.pokemon.tipoPrincipal)) || (Agua.mataA(estadoAnterior.pokemon.tipoSecundario.getOrElse(Agua)))) => estadoAnterior.flatMap { pokemon => EstadoKO(estadoAnterior.pokemon, "El pokemon perdia contra el agua")}
        case _ => estadoAnterior.map { pokemon => actividadActual.ejecutar(pokemon) }
      }
      
      case AprenderAtaque(ataque) => estadoAnterior match {
        case _ if estadoAnterior.pokemon.algunTipoEs(ataque.tipo) || ataque.tipo == Normal => estadoAnterior.map { pokemon => actividadActual.ejecutar(pokemon) }
        case _ => estadoAnterior.flatMap { pokemon => EstadoKO(pokemon, "Pokemon se lastimo trantando de aprender ataque") }
      }
  
      case UsarAntidoto => estadoAnterior match {
        case EstadoEnvenenado(_) => estadoAnterior.flatMap { pokemon => EstadoNormal(pokemon) }
        case _ => estadoAnterior.map { pokemon => pokemon }
      }

      case UsarEther => estadoAnterior match {
        case EstadoKO(_, _) => estadoAnterior.map { pokemon => pokemon }
        case _ => estadoAnterior.flatMap { pokemon => EstadoNormal(pokemon) }
      }
      
      case Descansar => estadoAnterior match {
        case EstadoNormal(_) =>  estadoAnterior.flatMap { pokemon => if (pokemon.energia < pokemon.energiaMaxima / 2) EstadoDormido(pokemon) else EstadoNormal(pokemon) }
        case _ => estadoAnterior.map { pokemon => actividadActual.ejecutar(pokemon)}
      }
      
      case UsarPiedra(piedra) => estadoAnterior match {
        case _ if piedra.tipo.mataA(estadoAnterior.pokemon.tipoPrincipal) || piedra.tipo.mataA(estadoAnterior.pokemon.tipoSecundario.get) => estadoAnterior.flatMap { pokemon => EstadoEnvenenado(pokemon) }
        case _ => estadoAnterior.map { pokemon => actividadActual.ejecutar(pokemon) }
      }
        

      case otraActividad =>
        estadoAnterior.map { pokemon => actividadActual.ejecutar(pokemon)}
        
    }
  }
}
