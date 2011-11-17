/**
 * バイトコード不正例外.
 */
class IllegalByteCodeException(cause:String) extends RuntimeException(cause)

/**
 * バイトコード.
 */
class ByteCode {

    import scala.collection.mutable.ListBuffer
    import java.io.OutputStream
    import java.io.OutputStreamWriter
    import java.io.BufferedWriter
    
    private var insts = new ListBuffer[Inst]()

    def apply(index:Int):Inst = insts(index)

    def save(os:OutputStream) {
        val writer = new BufferedWriter(new OutputStreamWriter(os))
        insts.foreach { inst =>
            writer.write(inst.toString)
            writer.write("\n")
        }
        writer.flush()
    }

    def add(inst:Inst) {
        insts.append(inst)
    }

    def size():Int = insts.size

}
object ByteCode {

    import java.io.InputStream
    import scala.io.Source

    def load(is:InputStream):ByteCode = {

        val byteCode = new ByteCode()

        val source = Source.fromInputStream(is, "UTF8")
        for( line <- source.getLines ) {
            val arr = line.split(",")

            var inst:Inst = null
            
            try {
                arr(0) match {
                    case OpeCode.LIT =>
                        inst = new Lit(arr(1).toInt)
                    case OpeCode.LOD =>
                        inst = new Lod(arr(1).toInt, arr(2).toInt)
                    case OpeCode.STO =>
                        inst = new Sto(arr(1).toInt, arr(2).toInt)
                    case OpeCode.CAL =>
                        inst = new Cal(arr(1).toInt, arr(2).toInt)
                    case OpeCode.RET =>
                        inst = new Ret(arr(1).toInt, arr(2).toInt)
                    case OpeCode.ICT =>
                        inst = new Ict(arr(1).toInt)
                    case OpeCode.JMP =>
                        inst = new Jmp(arr(1).toInt)
                    case OpeCode.JPC =>
                        inst = new Jpc(arr(1).toInt)
                    case OpeCode.NEG =>
                        inst = new Neg()
                    case OpeCode.ADD =>
                        inst = new Add()
                    case OpeCode.SUB =>
                        inst = new Sub()
                    case OpeCode.MUL =>
                        inst = new Mul()
                    case OpeCode.DIV =>
                        inst = new Div()
                    case OpeCode.MOD =>
                        inst = new Mod()
                    case OpeCode.ODD =>
                        inst = new Odd()
                    case OpeCode.EQ =>
                        inst = new Eq()
                    case OpeCode.LS =>
                        inst = new Ls()
                    case OpeCode.GR =>
                        inst = new Gr()
                    case OpeCode.NEQ =>
                        inst = new Neq()
                    case OpeCode.LSEQ =>
                        inst = new Lseq()
                    case OpeCode.GREQ =>
                        inst = new Greq()
                    case OpeCode.WRT =>
                        inst = new Wrt()
                    case OpeCode.WRL =>
                        inst = new Wrl()
                    case OpeCode.HEN =>
                        inst = new Hen()
                    case OpeCode.OMA =>
                        inst = new Oma()
                    case OpeCode.TIH =>
                        inst = new Tih()
                    case _ =>
                        throw new IllegalByteCodeException(line)
                }
            } catch {
                case e:IllegalByteCodeException => throw e
                case other => throw new IllegalByteCodeException(other.toString)
            }
            byteCode.add(inst)
        }

        return byteCode
    }
}
