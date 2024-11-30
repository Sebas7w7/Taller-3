package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MultMatrizTest extends AnyFunSuite {
  val taller3 = new Taller3()

  val matriz1 = Vector(
    Vector(2, 4, 6, 8),
    Vector(10, 12, 14, 16),
    Vector(18, 20, 22, 24),
    Vector(26, 28, 30, 32)
  )

  val matriz2 = Vector(
    Vector(1, 3, 5, 7),
    Vector(9, 11, 13, 15),
    Vector(17, 19, 21, 23),
    Vector(25, 27, 29, 31)
  )

  val identidad = Vector(
    Vector(1, 0, 0, 0),
    Vector(0, 1, 0, 0),
    Vector(0, 0, 1, 0),
    Vector(0, 0, 0, 1)
  )

  val matrizCero = Vector.fill(4, 4)(0)

  test("Multiplicación de matrices") {
  val resultadoEsperado = Vector(
    Vector(340,380,420,460),
    Vector(756,860,964,1068),
    Vector(1172,1340,1508,1676),
    Vector(1588,1820,2052,2284)
  )
  assert(taller3.multMatriz(matriz1, matriz2) == resultadoEsperado)
}



  test("Multiplicación de una matriz con la matriz identidad") {
  val resultadoEsperado = matriz1
  assert(taller3.multMatriz(matriz1, identidad) == resultadoEsperado)
}


  
  test("Multiplicación de una matriz con una matriz de ceros") {
    assert(taller3.multMatriz(matriz2, matrizCero) == matrizCero)
  }

  
  test("Multiplicación de una matriz con una de valores negativos") {
    val matrizNegativa = Vector(
      Vector(-2, -4, -6, -8),
      Vector(-10, -12, -14, -16),
      Vector(-18, -20, -22, -24),
      Vector(-26, -28, -30, -32)
    )
    val resultado = Vector(
      Vector(-360, -400, -440, -480),
      Vector(-808, -912, -1016, -1120),
      Vector(-1256, -1424, -1592, -1760),
      Vector(-1704, -1936, -2168, -2400)
    )
    assert(taller3.multMatriz(matriz1, matrizNegativa) == resultado) // El resultado será una matriz 2x2
  }

  
  test("Multiplicación de matrices con valores extremos") {
    val matrizMaximos = Vector.fill(4, 4)(Int.MaxValue)
    val matrizMinimos = Vector.fill(4, 4)(Int.MinValue)
    val resultadoEsperado = Vector.fill(4, 4)(0)
    assert(taller3.multMatriz(matrizMaximos, matrizMinimos) == resultadoEsperado)
  }

}
