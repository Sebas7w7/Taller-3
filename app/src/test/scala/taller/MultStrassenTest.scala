package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MultStrassenTest extends AnyFunSuite {

  val taller3 = new Taller3()

  val matriz1 = Vector(
    Vector(2, 4, 6, 8),
    Vector(1, 3, 5, 7),
    Vector(9, 11, 13, 15),
    Vector(10, 12, 14, 16)
  )

  val matriz2 = Vector(
    Vector(1, 0, 0, 0),
    Vector(0, 1, 0, 0),
    Vector(0, 0, 1, 0),
    Vector(0, 0, 0, 1)
  )

  val matrizConUnos = Vector(
    Vector(1, 1, 1, 1),
    Vector(1, 1, 1, 1),
    Vector(1, 1, 1, 1),
    Vector(1, 1, 1, 1)
  )

  val matrizCero = Vector.fill(4, 4)(0)

  test("Multiplicación de matrices 4x4 con números positivos") {
    val resultado = taller3.multStrassen(matriz1, matriz2)
    println("Resultado de la multiplicación de matrices 4x4 con números positivos:")
    println(resultado)
    assert(resultado == matriz1)
  }

  test("Multiplicación de una matriz con la matriz identidad") {
    val resultadoEsperado = matriz1
    assert(taller3.multStrassen(matriz1, matriz2) == resultadoEsperado)
  }

  test("Multiplicación de una matriz con una matriz de ceros") {
    assert(taller3.multStrassen(matriz1, matrizCero) == matrizCero)
  }

  test("Multiplicación de matrices con valores cancelados") {
    val matrizMaximos = Vector.fill(4, 4)(Int.MaxValue)
    val matrizMinimos = Vector.fill(4, 4)(Int.MinValue)
    val resultadoEsperado = Vector.fill(4, 4)(0) // Se espera que los valores se cancelen
    assert(taller3.multStrassen(matrizMaximos, matrizMinimos) == resultadoEsperado)
  }

  test("Multiplicación de matrices con elementos negativos") {
    val matrizNegativa = Vector(
      Vector(-2, -4, -6, -8),
      Vector(-1, -3, -5, -7),
      Vector(-9, -11, -13, -15),
      Vector(-10, -12, -14, -16)
    )

    val resultado = taller3.multStrassen(matrizNegativa, matriz2)
    println("Resultado de la multiplicación de matrices con elementos negativos:")
    println(resultado)
    assert(resultado == matrizNegativa)
  }

  test("Multiplicación de matrices con elementos mixtos (positivos y negativos)") {
    val matrizMixta = Vector(
      Vector(2, -4, 6, -8),
      Vector(-1, 3, -5, 7),
      Vector(9, -11, 13, -15),
      Vector(-10, 12, -14, 16)
    )

    val resultado = taller3.multStrassen(matrizMixta, matriz2)
    println("Resultado de la multiplicación de matrices con elementos mixtos (positivos y negativos):")
    println(resultado)
    // Esperamos que el resultado sea igual a la matriz original, ya que multiplicamos por la identidad.
    assert(resultado == matrizMixta)
  }
}
