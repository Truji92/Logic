package app

import java.io.{File, PrintWriter}
import java.util

import context.{Context, Rule}
import polynomial.ImplicationRetractor
import polynomial.ImplicationRetractor.{CConj, CImpl, TracedImpl, V1, V2}
import types.Types.Atom

import scala.collection.JavaConversions._
import scala.io.Source


object ExperimentalRunner {

  val basePath = "resultsByIteration/"

  def main(args: Array[String]): Unit = {

    val size = 16
    val densities = 1 to 9 map(_.toDouble / 10)

    densities forEach { d =>
      println("\n===========================")
      println(s"Running Densitiy $d")
      val impls = genImplications(objects = size, attributes = size, density = d)
      val vars = genVars(size) zipWithIndex
      val name = s"size$size/density${d*10}"

      vars foreach { case (v, index) =>
        println(s"Removing #$index var")
        val resultPath = s"$name/$index/"
        if (index == 0)
          runTest(resultPath, impls, v, tests = TestBoth)
        else {
          val implsV1 = App.parseFromFile(new File(s"$basePath/$name/${index-1}/resultV1.txt"))
          val implsV2 = App.parseFromFile(new File(s"$basePath/$name/${index-1}/resultV2.txt"))
          if (implsV1.isRight) println(implsV1.right.get)
          runTest(resultPath, implsV1.left.get, v, TestV1)
          runTest(resultPath, implsV2.left.get, v, TestV2)
        }
      }

      genResumen(name, size)
    }
  }

  def runTest(testName: String, implications: Set[CImpl], v: Atom, tests: TestFor): Unit =
    runTest(testName, implications, List(v), tests)

  def runTest(testName: String, implications: Set[CImpl], vars: List[Atom], tests: TestFor): Unit = {

    val path = s"$basePath/$testName/"
    new File(path).mkdirs()

    if (tests == TestBoth) {
      saveCommands(path, vars)
      write(path + "input.txt", implications.mkString("\n"))
    }

    write(path + "vars.txt", vars.mkString(", "))

    if (tests == TestBoth || tests == TestV1) {
      println("Running V1")
      val (timeV1, resultV1) = timed {
        ImplicationRetractor.run(implications, vars, otter = false, trace = false, version = V1)
      }

      write(path + "resultV1.txt", resultV1.map(_._1.impl).mkString("\n"))
      write(path + "timeV1.txt", timeV1.toString)

      println(s"TimeV1: $timeV1")
    }

    if (tests == TestBoth || tests == TestV2) {
      println("Running V2")
      val (timeV2, resultV2) = timed {
        ImplicationRetractor.run(implications, vars, otter = false, trace = false, version = V2)
      }

      write(path + "resultV2.txt", resultV2.map(_._1.impl).mkString("\n"))
      write(path + "timeV2.txt", timeV2.toString)

      println(s"TimeV2: $timeV2")
    }

//    if (tests == TestBoth) {
//      val genericResult = List("nVars", "nRules", "timeV1", "timeV2").mkString("",", ","\n") + s"${vars.size},${implications.size},$timeV1, $timeV2"
//
//      write(path + "times.txt", genericResult)
//
//      println(genericResult)
//    }

  }

  sealed trait TestFor
  case object TestV1 extends TestFor
  case object TestV2 extends TestFor
  case object TestBoth extends TestFor

  def genResumen(path: String, size: Int) = {
    val head = s"Iteración | TiempoV1 | TiempoV2 | TamañoSalidaV1 | TamañoSalidaV2"

    val results = 0 until size map { i =>
      val ipath = (file: String) => s"$basePath/$path/$i/$file"

      val t1 = Source.fromFile(new File(ipath("timeV1.txt"))).getLines().next()
      val t2 = Source.fromFile(new File(ipath("timeV2.txt"))).getLines().next()
      val n1 = App.parseFromFile(new File(ipath("resultV1.txt"))).left.get.size
      val n2 = App.parseFromFile(new File(ipath("resultV2.txt"))).left.get.size

      s"$i  | $t1  | $t2  | $n1  | $n2 "
    }

    val initialSize = App.parseFromFile(new File(s"$basePath/$path/0/input.txt")).left.get.size

    val resumen = s"Tamaño Inicial: $initialSize \n $head \n" + results.mkString("","\n","")
    println(resumen)
    write(s"$basePath/$path/resumen.txt", resumen)
  }

  def saveCommands(path: String, vars: List[Atom]) = {
    write(path + "commands.txt", vars.mkString(path + "input.txt -v ", ",", " -t") )
  }

  def write(name: String, content: String) = {
    val pw = new PrintWriter(new File(name))
    pw.write(content)
    pw.close()
  }

  def parseImplications(rules: util.LinkedList[Rule]) = {
    Set(rules.toList: _*).map { rule =>
      val premise = CConj(rule.getPremise.toSet.map(Atom))
      val conclusion = CConj(rule.getConclusion.toSet.map(Atom))

      CImpl(premise, conclusion)
    }
  }

  def genImplications(size: Int) = {
    val cxt = new Context("Aleatorio", 100, size, 0.25D)
    val ll  = cxt.getStemBasis(0)

    parseImplications(ll)
  }

  def genImplications(objects: Int, attributes: Int, density: Double) = {
    val cxt = new Context("Aleatorio", objects, attributes, density)
    val ll  = cxt.getStemBasis(0)

    parseImplications(ll)
  }

  def genVars(size: Int) = scala.util.Random.shuffle((0 until size).map{n => Atom("A"+n)}.toList)

}
