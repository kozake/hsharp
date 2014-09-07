object Hentai {

    import java.io.{File, FileInputStream}

    def printUsage() {
        println("Usage: FizzBuzzC <source file>")
    }

    def main(args:Array[String]) {

        if (args.length < 1 || !args(0).endsWith(".hs")) {
            printUsage()
            return
        }

        val source = loadSource(new File(args(0)))
        val byteCode = new Compiler().compile(source)
        
        val vm = new VM()
        vm.execute(byteCode)
    }

    private def loadSource(file:File):Source = {

        var fis:FileInputStream = null

        try {
            fis = new FileInputStream(file)

            return Source.load(fis)

        } finally {
            if (fis != null) {
                fis.close()
            }
        }
    }
}
