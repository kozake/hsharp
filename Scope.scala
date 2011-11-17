import scala.collection.mutable.ListBuffer

/**
 * 項目種別.
 */
class ScopeItemKind extends Enumeration
object ScopeItemKind extends Enumeration {
  val constant, variant, parameter, function = Value
}

/**
 * 項目.
 */
abstract class ScopeItem(val name:String, val kind:ScopeItemKind.Value)

/**
 * アドレス項目.
 */
abstract class ScopeAddrItem(name:String, kind:ScopeItemKind.Value, val level:Int, var address:Int) extends ScopeItem(name, kind)

/**
 * 定数項目.
 */
class ScopeConstantItem(name:String, val value:Int) extends ScopeItem(name, ScopeItemKind.constant)

/**
 * 変数項目.
 */
class ScopeVariantItem(name:String, level:Int, address:Int) extends ScopeAddrItem(name, ScopeItemKind.variant, level, address)

/**
 * パラメータ項目.
 */
class ScopeParameterItem(name:String, level:Int, address:Int) extends ScopeAddrItem(name, ScopeItemKind.parameter, level, address)

/**
 * 関数項目.
 */
class ScopeFunctionItem(name:String, level:Int, address:Int) extends ScopeAddrItem(name, ScopeItemKind.function, level, address) {
    val parameters = new ListBuffer[ScopeParameterItem]()
}

/**
 * スコープ.
 */
class Scope(level:Int, var localAddr:Int) {

    private val scopeItems = new ListBuffer[ScopeItem]()

    var currFunc:ScopeFunctionItem = null

    def addFunction(name:String, addr:Int):ScopeFunctionItem = {
        val item = new ScopeFunctionItem(name, level, addr)
        scopeItems.append(item)
        currFunc = item
        item
    }

    def addParameter(name:String):ScopeParameterItem = {
        val item = new ScopeParameterItem(name, level, 0)
        scopeItems.append(item)
        item
    }

    def addVariant(name:String):ScopeVariantItem = {
        val item = new ScopeVariantItem(name, level, localAddr)
        localAddr += 1
        scopeItems.append(item)
        item
    }

    def addConstant(name:String, value:Int):ScopeConstantItem = {
        val item = new ScopeConstantItem(name, value)
        scopeItems.append(item)
        item
    }

    def findItem(name:String):Option[ScopeItem] = scopeItems.find(_.name == name)

    def apply(index:Int):ScopeItem = scopeItems(index)
}

/**
 * スコープ管理.
 */
class ScopeManager {

    private var currLv:Int = -1

    private val scopes = new ListBuffer[Scope]

    def enterScope(firstAddr:Int) {
        currLv += 1
        scopes.append(new Scope(currLv, firstAddr))
    }

    def endScope() {
        scopes.remove(currLv)
        currLv -= 1
    }

    def endParameter() {
        var i = 0
        for (par <- currFunc.parameters) {
            par.address = i - currFunc.parameters.size
            i += 1
        }
    }

    def addFunction(name:String, addr:Int):ScopeFunctionItem = scopes(currLv).addFunction(name, addr)

    def addParameter(name:String):ScopeParameterItem = {
        val item = scopes(currLv).addParameter(name)
        currFunc.parameters.append(item)
        item
    }

    def addVariant(name:String):ScopeVariantItem = scopes(currLv).addVariant(name)

    def addConstant(name:String, value:Int):ScopeConstantItem = scopes(currLv).addConstant(name, value)

    def findItem(name:String):Option[ScopeItem] = {
        var find:Option[ScopeItem] = None
        var level = currLv
        while (find.getOrElse(null) == null && level >= 0) {
            find = scopes(level).findItem(name)
            level -= 1
        }
        find
    }

    def localAddr():Int = scopes(currLv).localAddr

    def currFunc():ScopeFunctionItem = if (currLv == 0) null else scopes(currLv - 1).currFunc
    
    def currLevel():Int = currLv
}
