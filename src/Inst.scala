/**
 * 命令.
 */
abstract class Inst {

    def execute(vm:VM)
}

/**
 * 値部を持つ命令.
 */
abstract class ValueInst(var value:Int) extends Inst

/**
 * レベル部と番地部を持つ命令.
 */
abstract class AddressInst(var level:Int, var address:Int) extends Inst

