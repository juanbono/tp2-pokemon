package utn.frba.pokemon

// Los ataques de ejemplo como Reposar, Enfocarse y Endurecerse deberian ser clases creadas en nuestro modelo, 
// o instancias de Ataque donde se le pasa el comportamiento que hace el ataque + el efecto??
sealed trait Ataque {
  def tipo: Tipo
  def puntosDeAtaque: Int
  def maximoInicialPA: Int

  def ejecutar(e:Estado, exp: Int): Estado
}

case class AtaqueConEstado(val tipo: Tipo, val puntosDeAtaque: Int, val maximoInicialPA: Int, f: (Estado => Estado)) extends Ataque {

  def ejecutar(e: Estado, exp: Int): Estado = {
    val copia = e.map { p: Pokemon => p.copy(experiencia = p.experiencia + exp) } 
    f(copia)
  }
}

case class AtaqueSinEstado(val tipo: Tipo, val puntosDeAtaque: Int, val maximoInicialPA: Int, f: (Pokemon => Pokemon)) extends Ataque {

  def ejecutar(e: Estado, exp: Int): Estado = {
    val copia = e.map { p: Pokemon => p.copy(experiencia = p.experiencia + exp) }
    copia.map(f)
  }
}