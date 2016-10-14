package app

import java.io.File

import polynomial.ImplicationRetractor
import polynomial.ImplicationRetractor.{CConj, CImpl, TracedImpl}
import types.Types.Atom

import scala.collection.immutable.ListSet
import scala.io.Source

object App {

  def main(args: Array[String]): Unit = {

    parser.parse(args, Config()) match {
      case Some(config) =>
        val Config(inputFile, vars, otter, trace) = config

        parseFromFile(inputFile) match {
          case Right(errors) => println("\nError al parsear el fichero: \n " + errors.mkString("\n","\n","\n"))
          case Left(input) => {
            val vs = vars.map(Atom).toList
            val result = ImplicationRetractor.run(input, vs, otter, trace)
            println("\n====================================")
            println("RESULTADO")
            println("====================================\n")
            println(s"Tamaño ${result.size}")
            println(result.map{
              case (TracedImpl(_, elem), index) => s"$index. $elem "
            }.mkString("\n","\n", "\n"))

            if (otter) {
              println("Otter")
              println(result.map{
                case (elem, index) => elem.toOtter
              }.mkString("\n", "\n", "\n"))
            }
            println("\n====================================\n")
          }
        }

      case None => println("\n")
    }

  }

  case class Config(
    inputFile: File = new File("."),
    vars: Seq[String] = Seq(),
    otterOutput: Boolean = false,
    trace: Boolean = false
  )

  val parser = new scopt.OptionParser[Config]("Implication Retractor") {
    head("Implication Retractor", "1.0")

    arg[File]("<file>").required().action( (file, config) =>
      config.copy(inputFile = file)
    ).text("Fichero que contiene las fórmulas")

    opt[Seq[String]]('v', "vars").required().action( (_vars, config) =>
      config.copy(vars=_vars)
    ).text("Lista de variables a eliminar")

    opt[Unit]('o', "otter").action( (_, config) =>
      config.copy(otterOutput = true)
    ).text("Mostrar salida en formato Otter")

    opt[Unit]('t', "trace").action( (_, config) =>
      config.copy(trace = true)
    ).text("Mostrar traza de la ejecución")

    override def showUsageOnError: Boolean = true
  }

  case class ParseError(msg: String) {
    override def toString = msg
  }

  def parseFromFile(file: File): Either[ListSet[CImpl], List[ParseError]] = {
    val lines = Source.fromFile(file).getLines()

    val ( result, errors ) = lines
      .map(_.replaceAll(" ", ""))
      .filterNot(_ == "")
      .map(parseLine)
      .partition(_.isLeft)

    if (errors.nonEmpty)
      Right(errors.collect{case Right(error) => error}.toList)
    else
      Left(ListSet.empty ++ result.collect{case Left(impl) => impl})
  }

  val pattern = """\[(\S+)\]=>\[(\S+)\]""".r
  def parseLine(line: String):Either[CImpl, ParseError] = line match {
    case pattern(pre, con) => parseFormula(pre, con) match {
      case Some(implication) => Left(implication)
      case None => Right(ParseError(s"Error al parsear $line"))
    }
    case _ => Right(ParseError(s"Error al parsear $line"))
  }


  def parseFormula(pre: String, con: String) = {
    val left = parseVars(pre)
    val right = parseVars(con)

    if (List(left, right).contains(None)) None
    else Some(left.get -> right.get)
  }

  val varsPattern = """(([\w]+,)*\w+)"""
  def parseVars(vars: String) = {
    if (vars.matches(varsPattern))
      Some(CConj(vars.split(",").map(Atom).toSet))
    else
    None

  }
}
