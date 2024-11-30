package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MultMatrizRecTest extends AnyFunSuite {
    
    val taller3 = new Taller3()

    // Definición de matrices de prueba comunes
    val matrizIdentidad = Vector(
        Vector(1, 0, 0, 0),
        Vector(0, 1, 0, 0),
        Vector(0, 0, 1, 0),
        Vector(0, 0, 0, 1)
    )

    val matrizCero = Vector.fill(4, 4)(0)

    val matriz1 = Vector(
        Vector(1, 2, 3, 4),
        Vector(5, 6, 7, 8),
        Vector(9, 10, 11, 12),
        Vector(13, 14, 15, 16)
    )

    val matriz2 = Vector(
        Vector(4, 3, 2, 1),
        Vector(8, 7, 6, 5),
        Vector(12, 11, 10, 9),
        Vector(16, 15, 14, 13)
    )

    val matrizGrande1 = Vector.fill(8, 8)(1)
    val matrizGrande2 = Vector.fill(8, 8)(2)
    val resultadoGrande = Vector.fill(8, 8)(16)

    test("Multiplicación recursiva secuencial identidad") {
        assert(taller3.multMatrizRec(matriz1, matrizIdentidad) == matriz1)
    }

    test("Multiplicación recursiva secuencial cero") {
        assert(taller3.multMatrizRec(matriz1, matrizCero) == matrizCero)
    }

    test("Multiplicación recursiva secuencial con sí misma") {
        val resultadoEsperado = Vector(
            Vector(90, 100, 110, 120),
            Vector(202, 228, 254, 280),
            Vector(314, 356, 398, 440),
            Vector(426, 484, 542, 600)
        )
        assert(taller3.multMatrizRec(matriz1, matriz1) == resultadoEsperado)
    }


    test("Multiplicación recursiva secuencial: matrices grandes llenas de valores") {
        assert(taller3.multMatrizRec(matrizGrande1, matrizGrande2) == resultadoGrande)
    }

    test("Multiplicación recursiva secuencial 1x1") {
        val m1 = Vector(Vector(3))
        val m2 = Vector(Vector(4))
        val resultadoEsperado = Vector(Vector(12))
        assert(taller3.multMatrizRec(m1, m2) == resultadoEsperado)
    }

    test("Multiplicación recursiva secuencial: matrices grandes con valores aleatorios") {
        val random = new scala.util.Random(42)
        val m1 = Vector.fill(8, 8)(random.nextInt(10))
        val m2 = Vector.fill(8, 8)(random.nextInt(10))
        val resultadoEsperado = taller3.multMatriz(m1, m2)
        assert(taller3.multMatrizRec(m1, m2) == resultadoEsperado)
    }


    test("Multiplicacion recursiva paralela aleatoria de tamaño 8 x 16 y 16 x 8") {
        val random = new scala.util.Random(42) 
        val m1 = Vector.fill(8, 16)(random.nextInt(10))
        val m2 = Vector.fill(16, 8)(random.nextInt(10))
        assert(taller3.multMatrizRec(m1, m2) == taller3.multMatrizRecPar(m1, m2))
    }

    test("Multiplicación recursiva secuencial y paralela producen el mismo resultado") {
        val random = new scala.util.Random(42) 
        val m1 = Vector.fill(4, 4)(random.nextInt(10))
        val m2 = Vector.fill(4, 4)(random.nextInt(10))
        assert(taller3.multMatrizRec(m1, m2) == taller3.multMatrizRecPar(m1, m2))
    }

}
