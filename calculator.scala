package org.cse97b.hw5
import scala.util.parsing.combinator._



/*
value::= Floatingpoint
expression::=term {"+" term | "-" term}
term ::= efactor { "*" efactor | "/" efactor}
efactor ::= factor | factor "^" factor 
factor::= value | "log("expression")" | "exp("expression")" | "("expression")"
*/

sealed abstract class ASTElement

case class Value(number: Double) extends ASTElement  {override def toString = s"$number"}
case class Log(exp: ASTElement) extends ASTElement  {override def toString = s"Log($exp)"}
case class Exp(exp: ASTElement) extends ASTElement  {override def toString = s"Exp($exp)"}

case class Power(f1: ASTElement, f2: ASTElement)  extends ASTElement {override def toString = s"($f1)^($f2)"}
case class Add(t1: ASTElement, t2: ASTElement)  extends ASTElement {override def toString = s"($t1) + ($t2)"}
case class Subtract(t1: ASTElement, t2: ASTElement)  extends ASTElement {override def toString = s"($t1) - ($t2)"}
case class Multiply(t1: ASTElement, t2: ASTElement)  extends ASTElement  {override def toString = s"($t1) * ($t2)"}
case class Divide(t1: ASTElement, t2: ASTElement)  extends ASTElement  {override def toString = s"($t1) / ($t2)"}

object Calculator extends JavaTokenParsers {
    class ParserError(message: String) extends Exception(message)
    def value: Parser[Value] = floatingPointNumber ^^ {x => Value(x.toDouble)} | failure("Error parsing value")
    def factor: Parser[ASTElement] = value   | 
                                    "log" ~> "(" ~> expression <~ ")"  ^^ {case x => Log(x)} |
                                    "exp" ~> "(" ~> expression <~ ")"  ^^ {case x => Exp(x)} |
                                    "(" ~> expression <~ ")" | failure("Error parsing factor")
    def efactor: Parser[ASTElement] = factor ~ opt("^" ~> factor) ^^ {case a~None => a; case a~Some(b) => Power(a,b)} | failure("unknown efactor")
    def term: Parser[ASTElement] = efactor ~ rep("*" ~ efactor | "/" ~ efactor) ^^ { 
                                                                                      case x~Nil => x 
                                                                                      case x~y => y.foldLeft(x) {
                                                                                         case (accum, "*"~z) => Multiply(accum, z)
                                                                                         case (accum, "/"~z) => Divide(accum, z)
                                                                                         case _ => throw new Exception("unrecognized term")
                                                                                      }
                                                                                    } | failure("Error parsing term")
    def expression: Parser[ASTElement] = term ~ rep("+" ~ term | "-" ~ term) ^^ { 
                                                                                  case x~Nil => x 
                                                                                  case x~y => y.foldLeft(x) {
                                                                                       case (accum, "+"~z) => Add(accum, z)
                                                                                       case (accum, "-"~z) => Subtract(accum, z)
                                                                                       case _ => throw new Exception("unrecognized expression")
                                                                                  }
                                                                                 } | failure("Error parsing expression")
    def apply(s: String) = {
        parseAll(expression, s) match {
            case Success(result, _) => result
            case failure: NoSuccess => throw new ParserError(failure.toString)
        }
    }
} 
  