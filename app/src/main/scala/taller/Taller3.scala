package taller

import common._

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

    // Versión recursiva de la multiplicación de matrices
    def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
        val n = m1.length
        if (n == 1) {
            // Caso base: multiplicación de un solo elemento
            Vector(Vector(m1(0)(0) * m2(0)(0)))
        } else {
            // Dividir las matrices en submatrices de tamaño n/2
            val half = n / 2

            val (a11, a12, a21, a22) = (
                subMatriz(m1, 0, 0, half),
                subMatriz(m1, 0, half, half),
                subMatriz(m1, half, 0, half),
                subMatriz(m1, half, half, half)
            )
            val (b11, b12, b21, b22) = (
                subMatriz(m2, 0, 0, half),
                subMatriz(m2, 0, half, half),
                subMatriz(m2, half, 0, half),
                subMatriz(m2, half, half, half)
            )

            // Calcular las submatrices de C
            val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
            val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
            val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
            val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

            // Construir la matriz resultante
            Vector.tabulate(n, n) { (i, j) =>
                if (i < half && j < half) c11(i)(j)
                else if (i < half) c12(i)(j - half)
                else if (j < half) c21(i - half)(j)
                else c22(i - half)(j - half)
            }
        }
    }


 
}
