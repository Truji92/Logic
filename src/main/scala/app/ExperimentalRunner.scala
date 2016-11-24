package app

import java.io.{File, PrintWriter}
import java.util

import context.{Context, Rule}
import polynomial.ImplicationRetractor
import polynomial.ImplicationRetractor.{CConj, CImpl, TracedImpl, V1, V2}
import types.Types.Atom

import scala.collection.JavaConversions._
import scala.collection.immutable.ListSet


object ExperimentalRunner {

  val basePath = "results/"

  def main(args: Array[String]): Unit = {

    var size = 5

    while (size <= 11) {
      runTest(size)
      size = size + 1
    }

  }

  def runTest(size: Int) = {
    val impls = genImplications(size)
    val vars = genVars(size).take(size-2)

    val path = basePath + size + "/"
    new File(path).mkdirs()

    saveCommands(path, vars)
    write(path + "input.txt", impls.mkString("\n"))
    write(path + "vars.txt", vars.mkString(", "))

    val (timeV1, resultV1) = timed {
      ImplicationRetractor.run(impls, vars, otter = false, trace = false, version = V1)
    }

    write(path + "resultV1.txt", resultV1.map(_._1.impl).mkString("\n"))
    write(path + "timeV1.txt", timeV1.toString)

    val (timeV2, resultV2) = timed {
      ImplicationRetractor.run(impls, vars, otter = false, trace = false, version = V2)
    }

    write(path + "resultV2.txt", resultV2.map(_._1.impl).mkString("\n"))
    write(path + "timeV2.txt", timeV2.toString)

    write(path + "times.txt", List("nVars", "nRules", "timeV1", "timeV2").mkString("",", ","\n") + s"$size,${impls.size},$timeV1, $timeV2" )

    println(resultV2)

  }

  def saveCommands(path: String, vars: List[Atom]) = {
    write(path + "commands.txt", vars.mkString(path + "result.txt -v ", ",", " -t") )
  }

  def write(name: String, content: String) = {
    val pw = new PrintWriter(new File(name))
    pw.write(content)
    pw.close()
  }

  def parseImplications(rules: util.LinkedList[Rule]) = {
    ListSet(rules.toList: _*).map { rule =>
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

  def genVars(size: Int) = scala.util.Random.shuffle((0 until size).map{n => Atom("A"+n)}.toList)

  def timed[T](block: => T): (Long, T) = {
    val start = System.currentTimeMillis()
    val result = block
    val end = System.currentTimeMillis()
    (end-start, result)
  }

}
