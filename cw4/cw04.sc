// Compiler for the JVM
//====================== 


// fresh variables
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// for JVM instructions and labels
extension (sc: StringContext) {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
}


// environment where variables are stored
type Env = Map[String, Int]

// you can make changes to the arguments of the auxiliary
// compile_* functions, but the main compile needs to take 
// a block and a class_name as argument, and produce the 
// j-file as string.

def compile_aexp(a: AExp, env : Env) : String = ???

def compile_bexp(b: BExp, env : Env, jmp: String) : String = ???

def compile_stmt(s: Stmt, env: Env) : (String, Env) = ???

def compile_block(bl: Block, env: Env) : (String, Env) = ???

def compile(bl: Block, class_name: String) : String = ???





def test_string(prog: String, cname: String) = {
  val tks = tokenise(prog) // has to adapted from earlier CWs
  //println(tks)
  val ast = Stmts.parse_all(tks).head // has to adapted from earlier CWs
  //println(ast)
  val code = compile(ast, cname)
  println(code)
  os.write.over(os.pwd / (cname ++ ".j"), code)
}

