package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class FuncionesMatricesTest extends AnyFunSuite {

  // Tests para la función prodPunto
  test("Producto punto de dos vectores normales") {
    assert(App.prodPunto(Vector(1, 2, 3), Vector(4, 5, 6)) == 32)
  }

  test("Producto punto de vectores con ceros") {
    assert(App.prodPunto(Vector(0, 0, 0), Vector(4, 5, 6)) == 0)
  }

  test("Producto punto de vectores unitarios") {
    assert(App.prodPunto(Vector(1, 1, 1), Vector(1, 1, 1)) == 3)
  }

  test("Producto punto de vectores con valores negativos") {
    assert(App.prodPunto(Vector(-1, -2, -3), Vector(4, 5, 6)) == -32)
  }

  test("Producto punto de vectores con un solo elemento") {
    assert(App.prodPunto(Vector(5), Vector(7)) == 35)
  }

  // Tests para la función transpuesta
  test("Transpuesta de una matriz cuadrada") {
    assert(App.transpuesta(Vector(
      Vector(1, 2, 3),
      Vector(4, 5, 6),
      Vector(7, 8, 9)
    )) == Vector(
      Vector(1, 4, 7),
      Vector(2, 5, 8),
      Vector(3, 6, 9)
    ))
  }

  test("Transpuesta de una matriz rectangular") {
    assert(App.transpuesta(Vector(
      Vector(1, 2),
      Vector(3, 4),
      Vector(5, 6)
    )) == Vector(
      Vector(1, 3, 5),
      Vector(2, 4, 6)
    ))
  }

  test("Transpuesta de una matriz de una sola fila") {
    assert(App.transpuesta(Vector(Vector(1, 2, 3))) == Vector(Vector(1), Vector(2), Vector(3)))
  }

  test("Transpuesta de una matriz de una sola columna") {
    assert(App.transpuesta(Vector(Vector(1), Vector(2), Vector(3))) == Vector(Vector(1, 2, 3)))
  }

  test("Transpuesta de una matriz vacía") {
    assert(App.transpuesta(Vector()) == Vector())
  }

  // Tests para la función multMatriz
  test("Multiplicación de matrices 2x2") {
    assert(App.multMatriz(
      Vector(Vector(1, 2), Vector(3, 4)),
      Vector(Vector(5, 6), Vector(7, 8))
    ) == Vector(Vector(19, 22), Vector(43, 50)))
  }

  test("Multiplicación de matrices 3x3") {
    assert(App.multMatriz(
      Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)),
      Vector(Vector(9, 8, 7), Vector(6, 5, 4), Vector(3, 2, 1))
    ) == Vector(Vector(30, 24, 18), Vector(84, 69, 54), Vector(138, 114, 90)))
  }

  test("Multiplicación de matrices con tamaño incompatible") {
    assertThrows[IllegalArgumentException] {
      App.multMatriz(
        Vector(Vector(1, 2, 3)),
        Vector(Vector(4, 5))
      )
    }
  }

  test("Multiplicación de matrices 1x1") {
    assert(App.multMatriz(
      Vector(Vector(3)),
      Vector(Vector(4))
    ) == Vector(Vector(12)))
  }

  test("Multiplicación de matrices de dimensiones 2x3 y 3x2") {
    assert(App.multMatriz(
      Vector(Vector(1, 2, 3), Vector(4, 5, 6)),
      Vector(Vector(7, 8), Vector(9, 10), Vector(11, 12))
    ) == Vector(Vector(58, 64), Vector(139, 154)))
  }

  // Tests para la función multMatrizPar
  test("Multiplicación paralela de matrices 2x2") {
    assert(App.multMatrizPar(
      Vector(Vector(1, 2), Vector(3, 4)),
      Vector(Vector(5, 6), Vector(7, 8))
    ) == Vector(Vector(19, 22), Vector(43, 50)))
  }

  test("Multiplicación paralela de matrices 3x3") {
    assert(App.multMatrizPar(
      Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9)),
      Vector(Vector(9, 8, 7), Vector(6, 5, 4), Vector(3, 2, 1))
    ) == Vector(Vector(30, 24, 18), Vector(84, 69, 54), Vector(138, 114, 90)))
  }

  test("Multiplicación paralela de matrices con tamaño incompatible") {
    assertThrows[IllegalArgumentException] {
      App.multMatrizPar(
        Vector(Vector(1, 2, 3)),
        Vector(Vector(4, 5))
      )
    }
  }

  test("Multiplicación paralela de matrices 1x1") {
    assert(App.multMatrizPar(
      Vector(Vector(3)),
      Vector(Vector(4))
    ) == Vector(Vector(12)))
  }

  test("Multiplicación paralela de matrices de dimensiones 2x3 y 3x2") {
    assert(App.multMatrizPar(
      Vector(Vector(1, 2, 3), Vector(4, 5, 6)),
      Vector(Vector(7, 8), Vector(9, 10), Vector(11, 12))
    ) == Vector(Vector(58, 64), Vector(139, 154)))
  }
}
