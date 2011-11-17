/**
 * スタック.
 */
class Stack(memorySize:Int) {

    private var memory = new Array[Int](memorySize)

    var top:Int = 0

    def push(value:Int) {
        memory(top) = value
        top += 1
        checkStackOverFlow
    }

    def pop():Int = {
        top -= 1
        memory(top)
    }

    def update(index:Int, value:Int):Unit = memory(index) = value

    def apply(index:Int):Int = memory(index)

    private def checkStackOverFlow():Unit = if (top >= memory.length) throw new OutOfMemoryError("stack overflow")
    
    override def toString():String = "top=[" + top + "], memory=" + memory.take(top + 10).mkString("[", ",", "]") 
}

/**
 * 仮想マシン.
 */
object VM {

    val MAX_STACK_MEMORY = 2000
    val MAX_DISPLAY_LEVEL = 5
}
class VM {

    val stack = new Stack(VM.MAX_STACK_MEMORY)
    val display = new Array[Int](VM.MAX_DISPLAY_LEVEL)
    var pc = 0
    var level = 0

    def execute(code:ByteCode) {

        pc = 0
        stack(0) = 0
        stack(1) = 0
        display(0) = 0

        do {
            var inst = code(pc)
            pc += 1
//println(inst)
            inst.execute(this)
//println("stack=[" + stack + "]"
//    + ", pc=[" + pc + "]"
//    + ", level=[" + level + "]"
//    + ", display=[" + display.mkString("[", ",", "]") + "]"
//)
//println("-------------------")
        } while (pc != 0)
    }
}
