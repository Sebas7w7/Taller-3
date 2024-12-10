package taller

import common._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.parallel.CollectionConverters._

class Taller3 {

  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val v = Vector.fill(long, long)(Random.nextInt(vals))
    v
  }

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    Vector.fill(long)(Random.nextInt(vals))
  }

  // Función para calcular el producto punto entre dos vectores
  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  def prodPuntoParD (v1: ParVector[Int], v2: ParVector[Int]): Int = {
    (v1 zip v2).map({case (i,j) => (i * j)}) . sum
  }

  // Función para calcular la transpuesta de una matriz
  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  // Versión estándar secuencial de la multiplicación de matrices
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val m2T = transpuesta(m2)
    val n = m1.length
    Vector.tabulate(n, n) { (i, j) =>
      prodPunto(m1(i), m2T(j))
    }
  }

  // Versión estándar paralela de la multiplicación de matrices
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val m2T = transpuesta(m2)
    Vector.tabulate(n, m2T(0).length) { (i, j) =>
      val taskElement = task {
        prodPunto(m1(i), m2T(j))
      }
      taskElement.join()
    }
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


  // Versión paralela de la multiplicación de matrices con la versión recursiva
  def multMatrizRecPar(m1: Matriz, m2: Matriz, umbral: Int = 64): Matriz = {
    val n = m1.length
    if (n <= umbral) {
      // Caso base: usar la versión secuencial
      multMatrizRec(m1, m2)
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

      // Paralelizar el cálculo de las submatrices
      val (c11, c12, c21, c22) = parallel(
        sumMatriz(multMatrizRecPar(a11, b11, umbral), multMatrizRecPar(a12, b21, umbral)),
        sumMatriz(multMatrizRecPar(a11, b12, umbral), multMatrizRecPar(a12, b22, umbral)),
        sumMatriz(multMatrizRecPar(a21, b11, umbral), multMatrizRecPar(a22, b21, umbral)),
        sumMatriz(multMatrizRecPar(a21, b12, umbral), multMatrizRecPar(a22, b22, umbral))
      )

      // Combinar las submatrices en la matriz resultante usando Vector.tabulate
      Vector.tabulate(n, n) { (i, j) =>
        if (i < half && j < half) c11(i)(j)
        else if (i < half) c12(i)(j - half)
        else if (j < half) c21(i - half)(j)
        else c22(i - half)(j - half)
      }
    }
  }

  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) - m2(i)(j))
  }

  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    // Verificar que las matrices son cuadradas y tienen dimensiones iguales
    val n = m1.length
    require(n == m2.length && (n & (n - 1)) == 0)

    if (n == 1) {
      // Caso base: multiplicación de un único elemento
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

      // Calcular las 7 multiplicaciones de Strassen
      val p1 = multStrassen(a11, restaMatriz(b12, b22))
      val p2 = multStrassen(sumMatriz(a11, a12), b22)
      val p3 = multStrassen(sumMatriz(a21, a22), b11)
      val p4 = multStrassen(a22, restaMatriz(b21, b11))
      val p5 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p6 = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))
      val p7 = multStrassen(restaMatriz(a11, a21), sumMatriz(b11, b12))

      // Combinar los resultados para obtener las submatrices de la matriz resultante
      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      // Construir la matriz resultante combinando las submatrices
      Vector.tabulate(n, n) { (i, j) =>
        if (i < half && j < half) c11(i)(j)
        else if (i < half) c12(i)(j - half)
        else if (j < half) c21(i - half)(j)
        else c22(i - half)(j - half)
      }
    }
  }

  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    // Verificar que las matrices son cuadradas y tienen dimensiones iguales
    val n = m1.length
    require(n == m2.length && (n & (n - 1)) == 0)

    if (n == 1) {
      // Caso base: multiplicación de un único elemento
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

      // Paralelizar las 7 multiplicaciones de Strassen en dos grupos
      val (p1, p2, p3, p4) = parallel(
        multStrassenPar(a11, restaMatriz(b12, b22)),
        multStrassenPar(sumMatriz(a11, a12), b22),
        multStrassenPar(sumMatriz(a21, a22), b11),
        multStrassenPar(a22, restaMatriz(b21, b11))
      )

      val (p5, temp) = parallel(
        multStrassenPar(sumMatriz(a11, a22), sumMatriz(b11, b22)),
        parallel(
          multStrassenPar(restaMatriz(a12, a22), sumMatriz(b21, b22)),
          multStrassenPar(restaMatriz(a11, a21), sumMatriz(b11, b12))
        )
      )

      val (p6, p7) = temp

      // Combinar los resultados de las submatrices
      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      // Reconstruir la matriz resultante
      Vector.tabulate(n, n) { (i, j) =>
        if (i < half && j < half) c11(i)(j)
        else if (i < half) c12(i)(j - half)
        else if (j < half) c21(i - half)(j)
        else c22(i - half)(j - half)
      }
    }
  }

}
