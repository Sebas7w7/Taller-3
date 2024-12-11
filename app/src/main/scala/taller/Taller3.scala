package taller

import common._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.parallel.CollectionConverters._
import org.scalameter._

class Taller3 {

  type Matriz = Vector[Vector[Int]]

  // Función para generar una matriz con valores aleatorios
  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val v = Vector.fill(long, long)(Random.nextInt(vals))
    v
  }

  // Función para generar un vector con valores aleatorios
  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    Vector.fill(long)(Random.nextInt(vals))
  }

  // Función para calcular el producto punto entre dos vectores
  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  // Función para calcular el producto punto entre dos vectores paralelamente
  def prodPuntoParD (v1: ParVector[Int], v2: ParVector[Int]): Int = {
    // Se realiza el producto punto entre los vectores v1 y v2 
    (v1 zip v2).map({case (i,j) => (i * j)}) . sum // Se suman los elementos del vector resultante
  }

  // Función para calcular la transpuesta de una matriz
  def transpuesta(m: Matriz): Matriz = {
    // Se obtiene la longitud de la matriz
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i)) 
  }

  // Versión estándar secuencial de la multiplicación de matrices
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val m2T = transpuesta(m2) // Se obtiene la transpuesta de la matriz m2
    val n = m1.length // Se obtiene la longitud de la matriz m1
    Vector.tabulate(n, n) { (i, j) => // Se genera una matriz de tamaño n x n
      prodPunto(m1(i), m2T(j)) // Se calcula el producto punto entre las filas de m1 y las columnas de m2
    }
  }

  // Versión estándar paralela de la multiplicación de matrices
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val m2T = transpuesta(m2) // Se obtiene la transpuesta de la matriz m2
    Vector.tabulate(n, m2T(0).length) { (i, j) => // Se genera una matriz de tamaño n x n
      val taskElement = task { // Se crea una tarea para calcular el producto punto entre las filas de m1 y las columnas de m2
        prodPunto(m1(i), m2T(j)) // Se calcula el producto punto entre las filas de m1 y las columnas de m2
      }
      taskElement.join() // Se espera a que la tarea termine y se retorna el resultado del producto punto
    }
  }

  // Función para dividir una matriz en submatrices
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l)((x, y) => m(i + x)(j + y))
  }

  // Función para sumar dos matrices
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) + m2(i)(j))
  }

  // Versión recursiva de la multiplicación de matrices
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length // Se obtiene la longitud de la matriz m1
    if (n == 1) {
      // Caso base: multiplicación de un solo elemento
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      // Dividir las matrices en submatrices de tamaño n/2
      val half = n / 2

      // Se obtienen las submatrices de m1 y m2
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
      Vector.tabulate(n, n) { (i, j) => // Se genera una matriz de tamaño n x n
        // Se combinan las submatrices en la matriz resultante usando Vector.tabulate
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
        // Se combinan las submatrices en la matriz resultante usando Vector.tabulate
        if (i < half && j < half) c11(i)(j)
        else if (i < half) c12(i)(j - half)
        else if (j < half) c21(i - half)(j)
        else c22(i - half)(j - half)
      }
    }
  }

  // Función para restar dos matrices
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) - m2(i)(j))
  }

  // Función para multiplicar dos matrices usando el algoritmo de Strassen
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

  // Versión paralela de la multiplicación de matrices con el algoritmo de Strassen
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
  // Function para realizar el benchmarking de las operaciones de multiplicación de matrices
  def compararAlgoritmos(
    funcionSecuencial: (Matriz, Matriz) => Matriz, // Función secuencial a evaluar
    funcionParalela: (Matriz, Matriz) => Matriz, // Función paralela a evaluar
    nombreSecuencial: String,
    nombreParalelo: String
  )(matriz1: Matriz, matriz2: Matriz): Unit = {
    val tiempoSecuencial = withWarmer(new Warmer.Default) measure { funcionSecuencial(matriz1, matriz2) } // Se mide el tiempo de ejecución de la función secuencial
    val tiempoParalelo = withWarmer(new Warmer.Default) measure { funcionParalela(matriz1, matriz2) } // Se mide el tiempo de ejecución de la función paralela
    
    // Se calcula la aceleración de la operación paralela con respecto a la secuencial
    val aceleracion = tiempoSecuencial.value / tiempoParalelo.value
    
    // Se imprimen los resultados
    // %.4f ms significa que se imprime el valor con 4 decimales de precisión en milisegundos
    println(f"\nTiempo $nombreSecuencial: ${tiempoSecuencial.value}%.4f ms") // tiempo de ejecución de la función secuencial
    println(f"Tiempo $nombreParalelo: ${tiempoParalelo.value}%.4f ms") // tiempo de ejecución de la función paralela
    println(f"Aceleración: $aceleracion%.4f") // Se imprime la aceleración de la operación paralela con respecto a la secuencial
  }

  // Function para comparar el producto punto entre dos vectores
  def compararProdPunto(
    funcionSecuencial: (Vector[Int], Vector[Int]) => Int, // Función secuencial a evaluar
    funcionParalela: (ParVector[Int], ParVector[Int]) => Int, // Función paralela a evaluar
    nombreSecuencial: String,
    nombreParalelo: String
  )(vector1: Vector[Int], vector2: Vector[Int]): Unit = {
    val tiempoSecuencial = withWarmer(new Warmer.Default) measure { funcionSecuencial(vector1, vector2) } // Se mide el tiempo de ejecución de la función secuencial
    val tiempoParalelo = withWarmer(new Warmer.Default) measure { funcionParalela(vector1.par, vector2.par) }// Se mide el tiempo de ejecución de la función paralela
    // Se mide el tiempo de ejecución de la función paralela para la función prodPuntoParD 
    val aceleracion = tiempoSecuencial.value / tiempoParalelo.value // Se calcula la aceleración de la operación paralela con respecto a la secuencial
    // Se imprimen los resultados
    println(f"\nTiempo $nombreSecuencial: ${tiempoSecuencial.value}%.4f ms") 
    println(f"Tiempo $nombreParalelo: ${tiempoParalelo.value}%.4f ms")
    println(f"Aceleración: $aceleracion%.4f")
  }
}


