/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package taller

object App {
  // Definición del tipo Matriz
  type Matriz = Vector[Vector[Int]]

  def greeting(): String = "Hello, World!"

  // Función para calcular el producto punto entre dos vectores
  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  // Función para calcular la transpuesta de una matriz
  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  // Versión estándar secuencial de la multiplicación de matrices
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val transpuestaM2 = transpuesta(m2)
    Vector.tabulate(m1.length, transpuestaM2.length)((i, j) =>
      prodPunto(m1(i), transpuestaM2(j))
    )
  }

  // Versión estándar paralela de la multiplicación de matrices
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val transpuestaM2 = transpuesta(m2)
    val tareas = for (i <- m1.indices) yield Future {
      Vector.tabulate(transpuestaM2.length)(j =>
        prodPunto(m1(i), transpuestaM2(j))
      )
    }
    val resultados = Await.result(Future.sequence(tareas), duration.Duration.Inf)
    Vector.tabulate(m1.length, m2.length)((i, j) =>
      resultados(i)(j)
    )
  }

  
}
