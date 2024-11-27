package taller

class Taller3 {
    
    type Matriz = Vector[Vector[Int]]
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

    def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l)((x, y) => m(i + x)(j + y))
    }

    def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) + m2(i)(j))
    }
  
}
