package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SumMatrizTest extends AnyFunSuite {
  type Matriz = Vector[Vector[Int]]
  val taller3 = new Taller3()


  val matriz1: Matriz = Vector(
    Vector(1, 2, 3, 4),
    Vector(5, 6, 7, 8),
    Vector(9, 10, 11, 12),
    Vector(13, 14, 15, 16)
  )

  val matriz2: Matriz = Vector(
    Vector(4, 3, 2, 1),
    Vector(8, 7, 6, 5),
    Vector(12, 11, 10, 9),
    Vector(16, 15, 14, 13)
  )

  test("Suma de dos matrices iguales") {
    val resultadoEsperado = Vector(
      Vector(2, 4, 6, 8),
      Vector(10, 12, 14, 16),
      Vector(18, 20, 22, 24),
      Vector(26, 28, 30, 32)
    )
    assert(taller3.sumMatriz(matriz1, matriz1) == resultadoEsperado)
  }

  test("Suma de dos matrices distintas") {
    val resultadoEsperado = Vector(
      Vector(5, 5, 5, 5),
      Vector(13, 13, 13, 13),
      Vector(21, 21, 21, 21),
      Vector(29, 29, 29, 29)
    )
    assert(taller3.sumMatriz(matriz1, matriz2) == resultadoEsperado)
  }

  test("Suma de matriz con una matriz de ceros") {
    val matrizCero: Matriz = Vector.fill(4, 4)(0)
    assert(taller3.sumMatriz(matriz1, matrizCero) == matriz1)
  }

  test("Suma de una matriz con una de valores negativos") {
    val matrizNegativa: Matriz = Vector(
      Vector(-1, -2, -3, -4),
      Vector(-5, -6, -7, -8),
      Vector(-9, -10, -11, -12),
      Vector(-13, -14, -15, -16)
    )
    val resultadoEsperado = Vector.fill(4, 4)(0)
    assert(taller3.sumMatriz(matriz1, matrizNegativa) == resultadoEsperado)
  }

  test("Suma de matrices con valores extremos") {
    val matrizMaximos: Matriz = Vector.fill(4, 4)(Int.MaxValue)
    val matrizMinimos: Matriz = Vector.fill(4, 4)(Int.MinValue)
    val resultadoEsperado = Vector.fill(4, 4)(-1)
    assert(taller3.sumMatriz(matrizMaximos, matrizMinimos) == resultadoEsperado)
  }
}

