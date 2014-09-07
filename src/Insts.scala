/**
 * オペコード.
 */
object OpeCode {

    val LIT = "lit"
    val LOD = "lod"
    val STO = "sto"
    val CAL = "cal"
    val RET = "ret"
    val ICT = "ict"
    val JMP = "jmp"
    val JPC = "jpc"
    val NEG = "neg"
    val ADD = "add"
    val SUB = "sub"
    val MUL = "mul"
    val DIV = "div"
    val MOD = "mod"
    val ODD = "odd"
    val EQ = "eq"
    val LS = "ls"
    val GR = "gr"
    val NEQ = "neq"
    val LSEQ = "lseq"
    val GREQ = "greq"
    val WRT = "wrt"
    val WRL = "wrl"
    val HEN = "hen"
    val OMA = "oma"
    val TIH = "tih"
}

/**
 * ADD命令.
 */
class Add extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push(s2 + s1)
    }
    
    override def toString():String = OpeCode.ADD
}

/**
 * CAL命令.
 */
class Cal(lv:Int, addr:Int) extends AddressInst(lv, addr) {

    def execute(vm:VM) {
        vm.level = level + 1
        vm.stack(vm.stack.top) = vm.display(vm.level)
        vm.stack(vm.stack.top + 1) = vm.pc
        vm.display(vm.level) = vm.stack.top
        vm.pc = address
    }

    override def toString():String = OpeCode.CAL + "," + level + "," + address

}

/**
 * DIV命令.
 */
class Div extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push(s2 / s1)
    }

    override def toString():String = OpeCode.DIV
}

/**
 * MOD命令.
 */
class Mod extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push(s2 % s1)
    }

    override def toString():String = OpeCode.MOD
}

/**
 * EQ命令.
 */
class Eq extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push( if (s2 == s1) 1 else 0 )
    }

    override def toString():String = OpeCode.EQ
}

/**
 * GR命令.
 */
class Gr extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push( if (s2 > s1) 1 else 0 )
    }

    override def toString():String = OpeCode.GR
}

/**
 * GREQ命令.
 */
class Greq extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push( if (s2 >= s1) 1 else 0 )
    }

    override def toString():String = OpeCode.GREQ
}

/**
 * ICT命令.
 */
class Ict(v:Int) extends ValueInst(v) {

    def execute(vm:VM) {
        vm.stack.top += value
    }

    override def toString():String = OpeCode.ICT + "," + value
}

/**
 * JMP命令.
 */
class Jmp(v:Int) extends ValueInst(v) {

    def execute(vm:VM) {
        vm.pc = value
    }

    override def toString():String = OpeCode.JMP + "," + value
}

/**
 * JPC命令.
 */
class Jpc(v:Int) extends ValueInst(v) {

    def execute(vm:VM) {
        val s = vm.stack.pop
        if (s == 0) {
            vm.pc = value
        }
    }

    override def toString():String = OpeCode.JPC + "," + value
}

/**
 * LIT命令.
 */
class Lit(v:Int) extends ValueInst(v) {

    def execute(vm:VM) {
        vm.stack.push(value)
    }

    override def toString():String = OpeCode.LIT + "," + value
}

/**
 * LOD命令.
 */
class Lod(lv:Int, addr:Int) extends AddressInst(lv, addr) {

    def execute(vm:VM) {
        val index = vm.display(level) + address
        vm.stack.push(vm.stack(index))
    }

    override def toString():String = OpeCode.LOD + "," + level + "," + address
}

/**
 * LS命令.
 */
class Ls extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push( if (s2 < s1) 1 else 0 )
    }

    override def toString():String = OpeCode.LS
}

/**
 * LSEQ命令.
 */
class Lseq extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push( if (s2 <= s1) 1 else 0 )
    }

    override def toString():String = OpeCode.LSEQ
}

/**
 * MUL命令.
 */
class Mul extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push( s2 * s1 )
    }

    override def toString():String = OpeCode.MUL
}

/**
 * NEG命令.
 */
class Neg extends Inst {

    def execute(vm:VM) {
        val s = vm.stack.pop
        vm.stack.push(-s)
    }

    override def toString():String = OpeCode.NEG
}

/**
 * NEQ命令.
 */
class Neq extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push( if (s1 != s2) 1 else 0 )
    }

    override def toString():String = OpeCode.NEQ
}

/**
 * ODD命令.
 */
class Odd extends Inst {

    def execute(vm:VM) {
        val s = vm.stack.pop
        vm.stack.push(s & 1)
    }

    override def toString():String = OpeCode.ODD
}

/**
 * RET命令.
 */
class Ret(lv:Int, addr:Int) extends AddressInst(lv, addr) {

    def execute(vm:VM) {
        // スタックの先頭が戻り値
        val ret = vm.stack.pop

        // スタックのtopを呼ばれたときの値に戻す
        vm.stack.top = vm.display(level)

        // 壊したディスプレイの回復
        vm.display(level) = vm.stack(vm.stack.top)

        vm.pc = vm.stack(vm.stack.top + 1)

        // 実引数分だけtopを戻す
        vm.stack.top = vm.stack.top - address

        vm.stack.push(ret)
    }

    override def toString():String = OpeCode.RET + "," + level + "," + address
}

/**
 * STO命令.
 */
class Sto(lv:Int, addr:Int) extends AddressInst(lv, addr) {

    def execute(vm:VM) {
        val s = vm.stack.pop
        vm.stack(vm.display(level) + address) = s
    }

    override def toString():String = OpeCode.STO + "," + level + "," + address
}

/**
 * SUB命令.
 */
class Sub extends Inst {

    def execute(vm:VM) {
        val s1 = vm.stack.pop
        val s2 = vm.stack.pop
        vm.stack.push(s2 - s1)
    }

    override def toString():String = OpeCode.SUB
}

/**
 * WRL命令.
 */
class Wrl extends Inst {

    def execute(vm:VM) {
        println
    }

    override def toString():String = OpeCode.WRL
}

/**
 * WRT命令.
 */
class Wrt extends Inst {

    def execute(vm:VM) {
        val s = vm.stack.pop
        print(s)
    }

    override def toString():String = OpeCode.WRT
}

/**
 * HEN命令.
 */
class Hen extends Inst {

    def execute(vm:VM) {
        print("jk<ｷｬｰ!!ﾍﾝﾀｲｰ!!")
    }

    override def toString():String = OpeCode.HEN
}

/**
 * OMA命令.
 */
class Oma extends Inst {

    def execute(vm:VM) {
        print("おまわりさん、こっちです！")
    }

    override def toString():String = OpeCode.OMA
}

/**
 * TIH命令.
 */
class Tih extends Inst {

    def execute(vm:VM) {
        print("変態<ﾀｲ━━━━||Φ|(|ﾟ|∀|ﾟ|)|Φ||━━━━ﾎ!!")
    }

    override def toString():String = OpeCode.TIH
}
