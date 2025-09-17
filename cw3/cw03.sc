// CW3
//=====



//
// The main idea is to extend the parser form the lectures 
// (which processes strings) to a parser that processes
// tokens. For this you need to use the lexer from CW2 and
// possibly adjust the lexing regular expressions accordingly.


// IMPORTANT:
//
// you need to include the lexer from CW2, which defines
// Tokens and tokenise
// 
// one way to do it is via the import statements
//
//import $file.^.cw2.cw02
//import cw02._
//
// or copy the code into this directory / file
// 


// parser combinators

type IsSeq[I] = I => Seq[_]

abstract class Parser[I, T](using is: IsSeq[I])  {
  def parse(in: I): Set[(T, I)]  

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in); 
        if is(tl).isEmpty) yield hd
}

// parser combinators

// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T], 
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)   
}

// sequence parser
class SeqParser[I: IsSeq, T, S](p: => Parser[I, T], 
                                q: => Parser[I, S]) extends Parser[I, (T, S)] {
  def parse(in: I) = 
    for ((hd1, tl1) <- p.parse(in); 
         (hd2, tl2) <- q.parse(tl1)) yield ((hd1, hd2), tl2)
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T], 
                         f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}


// some convenient syntax for parser combinators
extension [I: IsSeq, T](p: Parser[I, T]) {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}

// Abstract Syntax Trees
abstract class Stmt
abstract class AExp
abstract class BExp

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Read(s: String) extends Stmt
case class WriteVar(s: String) extends Stmt     // for printing values of variables
case class WriteString(s: String) extends Stmt  // for printing strings

case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class Lop(o: String, b1: BExp, b2: BExp) extends BExp
                 

// Parser rules
lazy val AExp: Parser[List[Token], AExp] = ???

lazy val BExp: Parser[List[Token], BExp] = ???

lazy val Stmt: Parser[List[Token], Stmt] = ???

lazy val Stmts: Parser[List[Token], Block] = ???

lazy val Block: Parser[List[Token], Block] = ???


// Interpreter
//=============

// Import needed to take an int as input from the user
import scala.io.StdIn.readInt

type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = ???
def eval_bexp(b: BExp, env: Env) : Boolean = ???
def eval_stmt(s: Stmt, env: Env) : Env = ???

def eval_bl(bl: Block, env: Env) : Env = ??? // for evaluating Blocks

// main eval function for the interpreter
def eval(bl: Block) : Env = eval_bl(bl, Map())



@main
def test(file: String) = {
  val contents = os.read(os.pwd / "examples" / file)
  println(s"Lex $file: ")
  val tks = tokenise(contents)
  println(tks.mkString(","))
  println(s"Parse $file: ")
  val ast = Stmts.parse_all(tks).head
  println(ast)
  println(s"Eval $file: ")
  println(eval(ast))
}
