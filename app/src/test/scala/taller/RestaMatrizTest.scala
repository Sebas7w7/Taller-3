package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RestaMatrizTest extends AnyFunSuite{

    val taller3 = new Taller3



    test("Resta de matrices 8x8 con valores grandes y pequeños aleatorios") {
        val m1 = Vector (
            Vector(724,110,791,656,74,544,604,862),
            Vector(293,579,154,424,189,245,439,770),
            Vector(805,81,338,909,782,112,774,189),
            Vector(330,769,143,583,283,305,239,351),
            Vector(446,360,978,847,22,92,690,421),
            Vector(277,280,781,923,285,970,691,496),
            Vector(584,230,801,750,443,536,693,510),
            Vector(302,355,594,989,147,531,325,635)
        ) 
        val m2 = Vector (
            Vector(81,689,916,886,425,881,626,376),
            Vector(323,440,557,745,81,712,658,806),
            Vector(330,357,35,978,614,344,497,800),
            Vector(356,967,261,174,1,0,361,321),
            Vector(334,865,288,44,463,952,484,472),
            Vector(525,550,462,693,25,647,699,42),
            Vector(797,703,761,604,396,983,280,629),
            Vector(738,179,495,649,638,954,571,509)
        )

        val resultado = taller3.restaMatriz(m1, m2) 

        val esperado = Vector (
            Vector(643,-579,-125,-230,-351,-337,-22,486),
            Vector(-30,139,-403,-321,108,-467,-219,-36),
            Vector(475,-276,303,-69,168,-232,277,-611),
            Vector(-26,-198,-118,409,282,305,-122,30),
            Vector(112,-505,690,803,-441,-860,206,-51),
            Vector(-248,-270,319,230,260,323,-8,454),
            Vector(-213,-473,40,146,47,-447,413,-119),
            Vector(-436,176,99,340,-491,-423,-246,126)

        )

        assert(resultado == esperado)

    }

    test("Resta de matrices con números alternados positivos y negativos 16x16") {
        val m1 = Vector.tabulate(16, 16)((i, j) => if ((i + j) % 2 == 0) 50 else -50)
        val m2 = Vector.tabulate(16, 16)((i, j) => if ((i + j) % 2 == 0) -50 else 50)
        val resultado = taller3.restaMatriz(m1, m2)

        val esperado = Vector.tabulate(16, 16)((i, j) => if ((i + j) % 2 == 0) 100 else -100)
        assert(resultado == esperado)
    }

    test("Resta de matrices grandes 32x32 con valores constantes") {
        val m1 = Vector.fill(32, 32)(100)
        val m2 = Vector.fill(32, 32)(50)
        val resultado = taller3.restaMatriz(m1, m2)

        val esperado = Vector.fill(32, 32)(50)
        assert(resultado == esperado)
    }

    test("Resta de matrices 4x4 derivadas de multiplicaciones") {
        val m1 = taller3.multMatriz(Vector(Vector(1, 2), Vector(3, 4)), Vector(Vector(5, 6), Vector(7, 8)))
        val m2 = taller3.multMatriz(Vector(Vector(2, 0), Vector(1, 2)), Vector(Vector(3, 1), Vector(4, 2)))
        val resultado = taller3.restaMatriz(m1, m2)

        val esperado = Vector.tabulate(2, 2)((i, j) => m1(i)(j) - m2(i)(j))
        assert(resultado == esperado)
    }

    test("Resta de matrices 64x64 con patrones generados") {
        val m1 = Vector.tabulate(64, 64)((i, j) => i + j)
        val m2 = Vector.tabulate(64, 64)((i, j) => i - j)
        val resultado = taller3.restaMatriz(m1, m2)

        val esperado = Vector.tabulate(64, 64)((i, j) => (i + j) - (i - j))
        assert(resultado == esperado)
    }
}
