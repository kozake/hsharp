object Compiler {
  val FirstAddr:Int = 2
}

class Compiler {

    private var token:Token = null
    private var source:Source = null
    private var byteCode:ByteCode = null
    private var scopeManager:ScopeManager = null

    def compile(src:Source):ByteCode = {

        byteCode = new ByteCode()
        scopeManager = new ScopeManager()

        source = src
        token = source.nextToken
        scopeManager.enterScope(Compiler.FirstAddr)
        mainblock()

        return byteCode
    }

    def mainblock() {

        val jmp = new Jmp(0)
        byteCode.add(jmp)

        var loop = true
        while (loop) {
            token.kind match {
                case TokenKind.Const =>
                    token = source.nextToken
                    constDecl()
                case TokenKind.Var =>
                    token = source.nextToken
                    varDecl()
                case TokenKind.Func =>
                    token = source.nextToken
                    funcDecl()
                case other => 
                    loop = false
                }
        }
        jmp.value = byteCode.size
        val ict = new Ict(0)
        byteCode.add(ict)

        while (token.kind != TokenKind.Nul) {
            statement()
            if (token.kind == TokenKind.Semicolon || token.kind == TokenKind.End) {
                token = source.nextToken
            }
        }
        ict.value = scopeManager.localAddr
        genRet()

        scopeManager.endScope()
    }

    def block() {

        val jmp = new Jmp(0)
        byteCode.add(jmp)

        var loop = true
        while (loop) {
            token.kind match {
                case TokenKind.Const =>
                    token = source.nextToken
                    constDecl()
                case TokenKind.Var =>
                    token = source.nextToken
                    varDecl()
                case TokenKind.Func =>
                    token = source.nextToken
                    funcDecl()
                case other => 
                    loop = false
                }
        }
        jmp.value = byteCode.size
        val func = scopeManager.currFunc()
        if (func != null) {
            func.address = byteCode.size
        }
        val ict = new Ict(0)
        byteCode.add(ict)
        statement()
        ict.value = scopeManager.localAddr
        genRet()

        scopeManager.endScope()
    }

    def constDecl() {

        var loop = true
        while (loop) {
            if (token.kind == TokenKind.Id) {
                val temp = token
                token = source.nextToken
                if (token.kind != TokenKind.Equal) {
                    printError(token, "Equal is not found.")
                }
                token = source.nextToken
                if (token.kind == TokenKind.Num) {
                    scopeManager.addConstant(temp.id, token.value)
                } else {
                    printError(token, "[" + token.id + "] Type should be Number.")
                }
                token = source.nextToken
            } else {
                printError(token, "[" + token.id + "] Type should be Id.")
            }
            if (token.kind != TokenKind.Comma) {
                loop = false
            } else {
              token = source.nextToken
            }
        }

        if (token.kind != TokenKind.Semicolon) {
            printError(token, "Semicolon is not found.")
        }
        token = source.nextToken
    }

    def varDecl() {
        var loop = true
        while(loop) {
            if (token.kind == TokenKind.Id) {
                scopeManager.addVariant(token.id)
                token = source.nextToken
            } else {
                printError(token, "[" + token.id + "] Type should be Id.")
            }
            if (token.kind != TokenKind.Comma) {
              loop = false
            } else {
                token = source.nextToken
            }
        }

        if (token.kind != TokenKind.Semicolon) {
            printError(token, "Semicolon is not found.")
        }
        token = source.nextToken
    }

    def funcDecl() {

        if (token.kind != TokenKind.Id) {
            printError(token, "Function name is not found.")
            return
        }

        scopeManager.addFunction(token.id, byteCode.size)

        token = source.nextToken
        if (token.kind != TokenKind.Lparen) {
            printError(token, "Lparen is not found.")
        }
        token = source.nextToken
        scopeManager.enterScope(Compiler.FirstAddr)

        var loop = true
        while(loop) {
            if (token.kind == TokenKind.Id) {
                scopeManager.addParameter(token.id)
                token = source.nextToken
            } else {
                loop = false
            }
            if (token.kind != TokenKind.Comma) {
                loop = false
            } else {
                token = source.nextToken
            }
        }
        if (token.kind != TokenKind.Rparen) {
            printError(token, "Rparen is not found.")
        }
        token = source.nextToken
        scopeManager.endParameter()
        block()
        if (token.kind != TokenKind.Semicolon) {
            printError(token, "Semicolon is not found.")
        }
        token = source.nextToken
    }

    def statement() {

        if (token.kind == TokenKind.Id) {
            var item = scopeManager.findItem(token.id).getOrElse(null)
            if (item == null) {
                // printError(token, token.id + " is not declared.")
                item = scopeManager.addVariant(token.id)
            }
            if (item.kind != ScopeItemKind.variant && item.kind != ScopeItemKind.parameter) {
                printError(token, token.id + " is not var/par.")
            }
            val addrItem = item.asInstanceOf[ScopeAddrItem]

            token = source.nextToken
            if (token.kind != TokenKind.Equal) {
                printError(token, "Equal is not found.")
            }
            token = source.nextToken

            expression()
            byteCode.add(new Sto(addrItem.level, addrItem.address))

        } else if (token.kind == TokenKind.If) {
            token = source.nextToken
            condition()
            // if (token.kind != TokenKind.Then) {
            //     printError(token, "Then is not found.")
            // }
            // token = source.nextToken

            val jpc = new Jpc(0)
            byteCode.add(jpc)
            statement()
            if (token.kind == TokenKind.Else) {
                token = source.nextToken
                val jmp = new Jmp(0)
                byteCode.add(jmp)
                jpc.value = byteCode.size
                statement()
                jmp.value = byteCode.size
            } else {
                jpc.value = byteCode.size
            }

        } else if (token.kind == TokenKind.Ret) {
            token = source.nextToken
            expression()
            genRet()

        } else if (token.kind == TokenKind.Begin) {
            token = source.nextToken

            var loop = true
            while (loop) {
                statement()
                if (token.kind == TokenKind.Semicolon) {
                    token = source.nextToken
                } else if (token.kind == TokenKind.End) {
                    token = source.nextToken
                    loop = false
                } else if (isStatementBeginKey(token)) {
                    printError(token, "Semicolon is not found.")
                } else {
                    printError(token, token.kind + " is invalid.")
                    loop = false
                    token = source.nextToken
                }
            }

        } else if (token.kind == TokenKind.While) {
            token = source.nextToken
            val addr = byteCode.size
            condition()
            // if (token.kind != TokenKind.Do) {
            //     printError(token, "Do is not found.")
            // }
            // token = source.nextToken

            val jpc = new Jpc(0)
            byteCode.add(jpc)
            statement()

            byteCode.add(new Jmp(addr))

            jpc.value = byteCode.size

        } else if (token.kind == TokenKind.For) {
            token = source.nextToken

            if (token.kind != TokenKind.Id) {
                printError(token, token.kind + " is invalid.")
                return
            }
            var item = scopeManager.findItem(token.id).getOrElse(null)
            if (item == null) {
                item = scopeManager.addVariant(token.id)
            }
            if (item.kind != ScopeItemKind.variant && item.kind != ScopeItemKind.parameter) {
                printError(token, token.id + " is not var/par.")
            }
            val addrItem = item.asInstanceOf[ScopeAddrItem]

            token = source.nextToken
            if (token.kind != TokenKind.Comma) {
                printError(token, token.kind + " is invalid.")
                return
            }
            token = source.nextToken
            if (token.kind != TokenKind.Num) {
                printError(token, "[" + token.kind + "] Type should be Number.")
                return
            }
            val start = token.value
            token = source.nextToken
            if (token.kind != TokenKind.Comma) {
                printError(token, token.kind + " is invalid.")
                return
            }
            token = source.nextToken
            if (token.kind != TokenKind.Num) {
                printError(token, "[" + token.kind + "] Type should be Number.")
                return
            }
            val end = token.value

            
            byteCode.add(new Lit(start))
            byteCode.add(new Sto(addrItem.level, addrItem.address))
            
            val addr = byteCode.size
            
            byteCode.add(new Lod(addrItem.level, addrItem.address))
            byteCode.add(new Lit(end))
            byteCode.add(new Lseq())

            val jpc = new Jpc(0)
            byteCode.add(jpc)
            token = source.nextToken
            statement()

            byteCode.add(new Lod(addrItem.level, addrItem.address))
            byteCode.add(new Lit(1))
            byteCode.add(new Add())
            byteCode.add(new Sto(addrItem.level, addrItem.address))

            byteCode.add(new Jmp(addr))

            jpc.value = byteCode.size

        } else if (token.kind == TokenKind.Write) {
            token = source.nextToken
            expression()
            byteCode.add(new Wrt())

        } else if (token.kind == TokenKind.Writeln) {
            token = source.nextToken
            byteCode.add(new Wrl())
        } else if (token.kind == TokenKind.Print) {
            token = source.nextToken
            expression()
            byteCode.add(new Wrt())
            byteCode.add(new Wrl())
        } else if (token.kind == TokenKind.Hen) {
            token = source.nextToken
            byteCode.add(new Hen())
            byteCode.add(new Wrl())
        } else if (token.kind == TokenKind.Oma) {
            token = source.nextToken
            byteCode.add(new Oma())
            byteCode.add(new Wrl())
        } else if (token.kind == TokenKind.Tih) {
            token = source.nextToken
            byteCode.add(new Tih())
            byteCode.add(new Wrl())
        } else if (token.kind == TokenKind.End) {
        } else if (token.kind == TokenKind.Semicolon) {
        } else {
            printError(token, token.kind + " is invalid.")
            token = source.nextToken
        }
    }

    def isStatementBeginKey(token:Token):Boolean = 
        token.kind == TokenKind.If ||
        token.kind == TokenKind.Begin ||
        token.kind == TokenKind.Ret ||
        token.kind == TokenKind.While ||
        token.kind == TokenKind.Write ||
        token.kind == TokenKind.Writeln

    def expression() {

        var kind = token.kind
        if (kind == TokenKind.Plus || kind == TokenKind.Minus) {
            token = source.nextToken
            term()
            if (kind == TokenKind.Minus) {
                byteCode.add(new Neq())
            }
        } else {
            term()
        }
        kind = token.kind
        while (kind == TokenKind.Plus || kind == TokenKind.Minus) {
            token = source.nextToken
            term()
            if (kind == TokenKind.Minus) {
                byteCode.add(new Sub())
            } else {
                byteCode.add(new Add())
            }
            kind = token.kind
        }
    }

    def term() {
        factor()
        var kind = token.kind
        while (kind == TokenKind.Mult || kind == TokenKind.Div || kind == TokenKind.Mod) {
            token = source.nextToken
            factor()
            if (kind == TokenKind.Mult) {
                byteCode.add(new Mul())
            } else if (kind == TokenKind.Div) {
                byteCode.add(new Div())
            } else if (kind == TokenKind.Mod) {
                byteCode.add(new Mod())
            }
            kind = token.kind
        }
    }

    def factor() {
        var kind = token.kind

        if (kind == TokenKind.Id) {

            var item = scopeManager.findItem(token.id).getOrElse(null)
            if (item == null) {
                // printError(token, token.id + " is not declared.")
                item = scopeManager.addVariant(token.id)
            }

            val itemKind = item.kind
            if (itemKind == ScopeItemKind.variant || itemKind == ScopeItemKind.parameter) {
                val addrItem = item.asInstanceOf[ScopeAddrItem]
                byteCode.add(new Lod(addrItem.level, addrItem.address))
                token = source.nextToken

            } else if (itemKind == ScopeItemKind.constant) {
                var constItem = item.asInstanceOf[ScopeConstantItem]
                byteCode.add(new Lit(constItem.value))
                token = source.nextToken

            } else if (itemKind == ScopeItemKind.function) {
                val funcItem = item.asInstanceOf[ScopeFunctionItem]

                token = source.nextToken

                if (token.kind == TokenKind.Lparen) {
                    var parCnt:Int = 0
                    token = source.nextToken
                    if (token.kind != TokenKind.Rparen) {
                        var loop = true
                        while (loop) {
                            expression()
                            parCnt += 1
                            if (token.kind == TokenKind.Comma) {
                                token = source.nextToken
                            } else {
                                if (token.kind != TokenKind.Rparen) {
                                    printError(token, "Rparen is not found.")
                                }
                                token = source.nextToken
                                loop = false
                            }
                        }
                    } else {
                        token = source.nextToken
                    }
                    if (funcItem.parameters.size != parCnt) {
                        printError(token, funcItem.name + " parameter count is unmatch.")
                    }
                } else {
                    printError(token, "Lparen is not found.")
                }
                byteCode.add(new Cal(funcItem.level, funcItem.address))
            }

        } else if (kind == TokenKind.Num) {
            byteCode.add(new Lit(token.value))
            token = source.nextToken

        } else if (kind == TokenKind.Lparen) {
            token = source.nextToken
            expression()
            if (token.kind != TokenKind.Rparen) {
                printError(token, "Rparen is not found.")
            }
            token = source.nextToken
        }
        kind = token.kind
        if (kind == TokenKind.Id || kind == TokenKind.Num || kind == TokenKind.Lparen) {
            printError(token, "Op is not found.")
            factor()
        } else {
            return
        }
    }

    def condition() {

        if (token.kind == TokenKind.Odd) {
            token = source.nextToken
            expression()
            byteCode.add(new Odd())
        } else {
            expression()
            val kind = token.kind
            if (kind != TokenKind.Equal
            && kind != TokenKind.Lss
            && kind != TokenKind.Gtr
            && kind != TokenKind.NotEq
            && kind != TokenKind.LssEq
            && kind != TokenKind.GtrEq) {
                // printError(token, "rel-op")
                
            } else {
                token = source.nextToken
                expression()
                if (kind == TokenKind.Equal) {
                    byteCode.add(new Eq())
                } else if (kind == TokenKind.Lss) {
                    byteCode.add(new Ls())
                } else if (kind == TokenKind.Gtr) {
                    byteCode.add(new Gr())
                } else if (kind == TokenKind.NotEq) {
                    byteCode.add(new Neq())
                } else if (kind == TokenKind.LssEq) {
                    byteCode.add(new Lseq())
                } else if (kind == TokenKind.GtrEq) {
                    byteCode.add(new Greq())
                }
            }
        }
    }

    private def genRet() {

      byteCode(byteCode.size - 1) match {
        case ret:Ret =>
        case other =>
            var ret:Ret = null
            if (scopeManager.currFunc != null) {
                ret = new Ret(scopeManager.currLevel, scopeManager.currFunc.parameters.size)
            } else {
                ret = new Ret(0, 0)
            }
            byteCode.add(ret)
        }
    }

    private def printError(token:Token, error:String) {
        source.printError(token, error)
    }
}
