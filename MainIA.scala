object MainIA{

  case class Estudiante(nombre: String, notas: List[Double])

  // ----------------------------------------------------------
  // MÉTODO QUE CALCULA EL ESTUDIANTE MÁS CONSISTENTE (FUNCIONAL)
  // ----------------------------------------------------------
  def estudianteMasConsistente(estudiantes: List[Estudiante]): Option[(String, Double)] = {

    // 1. Filtrar estudiantes con ≥ 3 notas
    def filtrarValidos(lista: List[Estudiante]): List[Estudiante] = lista match {
      case Nil => Nil
      case e :: resto =>
        if (e.notas.length >= 3)
          e :: filtrarValidos(resto)
        else
          filtrarValidos(resto)
    }

    val validos = filtrarValidos(estudiantes)

    if (validos.isEmpty)
      return None

    // 2. Funciones recursivas de máximo y mínimo
    def maximo(notas: List[Double]): Double = notas match {
      case Nil => Double.MinValue
      case x :: Nil => x
      case x :: xs =>
        val maxResto = maximo(xs)
        if (x > maxResto) x else maxResto
    }

    def minimo(notas: List[Double]): Double = notas match {
      case Nil => Double.MaxValue
      case x :: Nil => x
      case x :: xs =>
        val minResto = minimo(xs)
        if (x < minResto) x else minResto
    }

    // 3. Consistencia = max - min
    def consistencia(e: Estudiante): Double = {
      val max = maximo(e.notas)
      val min = minimo(e.notas)
      max - min
    }

    // 4. Seleccionar al que tenga la menor consistencia
    def buscarMejor(lista: List[Estudiante]): Estudiante = lista match {
      case x :: Nil => x
      case x :: xs =>
        val mejorResto = buscarMejor(xs)
        if (consistencia(x) < consistencia(mejorResto)) x else mejorResto
    }

    val mejor = buscarMejor(validos)
    Some((mejor.nombre, consistencia(mejor)))
  }


  // ----------------------------------------------------------
  // MAIN
  // ----------------------------------------------------------
  def main(args: Array[String]): Unit = {

    val estudiantes: List[Estudiante] = List(
      Estudiante("Ana",       List(8.5, 9.0, 9.2, 8.8, 9.1)),
      Estudiante("Juan",      List(7.5, 8.0, 7.8, 7.9, 8.1)),
      Estudiante("María",     List(9.5, 9.0, 8.9, 9.1, 9.2)),
      Estudiante("Carlos",    List(6.0, 7.0, 8.5, 5.5, 7.8)),
      Estudiante("Lucía",     List(9.0, 8.8, 9.2, 9.1, 8.9)),
      Estudiante("Pedro",     List(5.0, 6.0, 7.5, 7.8, 8.0)),
      Estudiante("Sofía",     List(8.8, 8.7, 8.9, 8.6, 8.8)),
      Estudiante("Diego",     List(7.2, 7.5, 7.8, 7.9, 7.0)),
      Estudiante("Elena",     List(9.1, 9.0, 9.3, 9.2, 9.4)),
      Estudiante("Raúl",      List(6.5, 6.8, 7.0, 7.2, 7.1)),
      Estudiante("Patricia",  List(8.0, 7.8, 8.2, 8.1, 8.0)),
      Estudiante("Fernando",  List(9.5, 8.5, 9.0, 8.8, 7.5)),
      Estudiante("Mónica",    List(8.2, 8.3, 8.1, 8.4, 8.3)),
      Estudiante("Héctor",    List(7.0, 7.2, 7.1, 7.3, 7.2)),
      Estudiante("Gabriela",  List(9.0, 9.1, 9.2, 9.0, 9.1)),
      Estudiante("Andrés",    List(6.8, 7.0, 6.9, 7.1, 7.0)),
      Estudiante("Valeria",   List(8.9, 9.0, 8.8, 8.9, 9.1)),
      Estudiante("Jorge",     List(7.5, 7.6, 7.4, 7.7, 7.6)),
      Estudiante("Carmen",    List(9.3, 9.2, 9.4, 9.5, 9.3)),
      Estudiante("Luis",      List(5.5, 6.0, 6.5, 6.8, 7.0)),
      Estudiante("Natalia",   List(8.0, 8.1, 8.2, 8.0, 8.1)),
      Estudiante("Sebastián", List(7.8, 8.0, 8.2, 7.9, 8.1))
    )

    val resultado = estudianteMasConsistente(estudiantes)

    resultado match {
      case Some((nombre, cons)) =>
        println(s"Estudiante más consistente: $nombre")
        println(f"Consistencia = $cons%.2f")
      case None =>
        println("No existen estudiantes con al menos 3 notas.")
    }
  }
}

