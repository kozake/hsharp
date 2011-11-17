class Source(source:String) {

    import scala.io.Source
    import java.io.InputStream

    private var currPos:Int = 0
    private var lineNum:Int = 1
    private var linePos:Int = 0

    def nextToken():Token = {

        var token:Token = null

        var ch = stepSpace()
        if (ch == 0) return new Token(TokenKind.Nul, lineNum, linePos)

        val ln = lineNum;
        val lp = linePos - 1;

        if (isLetter(ch)) {
          
            val buff = new StringBuffer
            do {
                buff.append(ch)
                ch = nextChar()
            } while (isLetter(ch) || isDigit(ch))

            val tokenKind = TokenKind.values.find( _.toString == buff.toString).getOrElse(TokenKind.Others)

            if (tokenKind != TokenKind.Others) {
                token = new Token(tokenKind, ln, lp)
            } else {
                token = new Token(TokenKind.Id, ln, lp)
                token.id = buff.toString
            }

        } else if (isDigit(ch)) {

            val buff = new StringBuffer
            var num:Int = 0
            do {
                if ((Int.MaxValue / 10 < num) || (Int.MaxValue - (ch - '0') < num * 10)) {
                    token = new Token(TokenKind.Num, ln, lp)
                    token.value = num
                    printError(token, "number is too large.")
                    return token
                }
                num = 10 * num + (ch - '0')
                buff.append(ch)
                ch = nextChar()
            } while (isDigit(ch))

            token = new Token(TokenKind.Num, ln, lp)
            token.value = num

        } else {

            var tokenKind = TokenKind.values.find( _.toString == String.valueOf(ch)).getOrElse(TokenKind.Others)
            ch = nextChar()

            if (tokenKind == TokenKind.Lss) {
                if (ch == '=') {
                    tokenKind = TokenKind.LssEq
                    ch = nextChar()
                } else if (ch == '>') {
                    tokenKind = TokenKind.NotEq
                    ch = nextChar()
                }
            } else if (tokenKind == TokenKind.Gtr) {
                if (ch == '=') {
                    tokenKind = TokenKind.GtrEq
                    ch = nextChar()
                }
            }

            token = new Token(tokenKind, ln, lp)
        }

        return token
    }

    private def stepSpace():Char = {
        if (source.length == currPos) return 0

        var lf = false
        var loop = true

        var ch = currChar()
        while (loop) {
            ch match {
                case 0 =>
                    loop = false
                case ' ' =>
                    lf = false
                case '\t' =>
                    lf = false
                case '\n' =>
                    lineNum += 1
                    linePos = 0
                    lf = true
                case '\r' =>
                    if(!lf) {
                        lineNum += 1
                        linePos = 0
                    }
                    lf = false
                case other =>
                    loop = false
            }
            if (loop) ch = nextChar()
        }
        return ch
    }

    private def currChar():Char = source.charAt(currPos)

    private def nextChar():Char = {

        if (source.length <= (currPos + 1)) {
            currPos = source.length
            0
        } else {
            linePos += 1
            currPos += 1
            source.charAt(currPos)
        }
    }

    private def isLetter(ch:Char):Boolean = ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))

    private def isDigit(ch:Char):Boolean = (ch >= '0' && ch <= '9')

    private var errorCnt:Int = 0
    def printError(token:Token, error:String) {
        println("["+token.lineNum+","+token.linePos+"]" + "ERROR::" + error)
        errorCnt += 1
    }
    def hasError:Boolean = errorCnt != 0
}

object Source {

    import java.io.InputStream

    def load(is:InputStream):Source = {
        val buff = new StringBuilder();
        var ch:Int = 0;

        ch = is.read
        while (ch != -1) {
            buff.append(ch.toChar);
            ch = is.read
        }

        new Source(buff.toString)
    }
}

