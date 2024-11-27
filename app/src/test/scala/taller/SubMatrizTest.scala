package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrizTest extends AnyFunSuite {
  type Matriz = Vector[Vector[Int]]

  val taller3 = new Taller3()


  val matriz: Matriz = Vector(
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

  // Test para subMatriz
  test("Extraer submatriz A11 de tamaño 2x2 desde la esquina superior izquierda") {
    val resultadoEsperado = Vector(
      Vector(1, 2),
      Vector(5, 6)
    )
    assert(taller3.subMatriz(matriz, 0, 0, 2) == resultadoEsperado)
  }

  test("Extraer submatriz A22 de tamaño 2x2 desde la esquina inferior derecha") {
    val resultadoEsperado = Vector(
      Vector(11, 12),
      Vector(15, 16)
    )
    assert(taller3.subMatriz(matriz, 2, 2, 2) == resultadoEsperado)
  }

  test("Extraer submatriz de tamaño 3x3 desde el centro de la matriz") {
    val resultadoEsperado = Vector(
      Vector(6, 7, 8),
      Vector(10, 11, 12),
      Vector(14, 15, 16)
    )
    assert(taller3.subMatriz(matriz, 1, 1, 3) == resultadoEsperado)
  }

  test("Extraer submatriz A21 de tamaño 2x2 desde la esquina inferior izquierda") {
    val resultadoEsperado = Vector(
      Vector(9, 10),
      Vector(13, 14)
    )
    assert(taller3.subMatriz(matriz, 2, 0, 2) == resultadoEsperado)
  }

  test("Extraer submatriz completa de tamaño 4x4") {
    assert(taller3.subMatriz(matriz, 0, 0, 4) == matriz)
  }
}