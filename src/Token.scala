/**
 * トークン種別.
 */
class TokenKind extends Enumeration
object TokenKind extends Enumeration {

    val Begin = Value("{")
    val End = Value("}")
    val If = Value("?")
    val Then = Value("then")
    val Else = Value("|")
    val While = Value("While")
    val For = Value("@")
    val Do = Value("do")
    val Ret = Value("return")
    val Func = Value("function")
    val Var = Value("var")
    val Const = Value("const")
    val Odd = Value("odd")
    val Write = Value("write")
    val Writeln = Value("writeln")
    val Hen = Value("h")
    val Oma = Value("o")
    val Tih = Value("t")
    val Print = Value("p")

    val Plus = Value("+")
    val Minus = Value("-")
    val Mult = Value("*")
    val Div = Value("/")
    val Mod = Value("%")
    val Lparen = Value("(")
    val Rparen = Value(")")
    val Equal = Value("=")
    val Lss = Value("<")
    val Gtr = Value(">")
    val NotEq = Value("<>")
    val LssEq = Value("<=")
    val GtrEq = Value(">=")
    val Comma = Value(",")
    val Period = Value(".")
    val Semicolon = Value(";")
    val Colon = Value(":")

    val Id = Value
    val Num = Value
    val Nul = Value
    val Letter = Value
    val Digit = Value

    val Others = Value
}

/**
 * トークン.
 */
class Token(val kind:TokenKind.Value, val lineNum:Int, val linePos:Int) {

    var id:String = null
    var value:Int = 0

    override def toString:String = kind + "," + id + "," + value + "," + lineNum + "," + linePos
}
