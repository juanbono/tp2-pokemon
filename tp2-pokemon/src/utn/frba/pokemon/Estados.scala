package utn.frba.pokemon

trait Estado

//Deberiamos considerar un estado normal? o es el No estado?
case object EstadoNormal extends Estado
case object EstadoDormido extends Estado
case object EstadoEnvenenado extends Estado
case object EstadoParalizado extends Estado
case object EstadoKO extends Estado