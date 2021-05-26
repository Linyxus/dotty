package dotty.tools.dotc.config

import util.DynamicVariable

object Printers {

  class Printer {
    def println(msg: => String): Unit = System.out.println(msg)
  }

  class ScopePrinter extends Printer {
    private val inScope: DynamicVariable[Boolean] = new DynamicVariable(false)

    def traceScope[T](body: => T): T =
      inScope.withValue(true) { body }

    def traceScopeWhen[T](cond: Boolean)(body: => T): T =
      if cond then traceScope { body }
      else body

    override def println(msg: => String): Unit =
      println("trying to print")
      if inScope.value then
        System.out.println(msg)
      else
        ()
  }

  object noPrinter extends Printer {
    inline override def println(msg: => String): Unit = ()
  }

  val default = new Printer

  val constr = noPrinter
  val core = noPrinter
  val checks = noPrinter
  val config = noPrinter
  val cyclicErrors = noPrinter
  val debug = noPrinter
  val derive = noPrinter
  val desugar = noPrinter
  val dottydoc = noPrinter
  val exhaustivity = noPrinter
  val gadts = noPrinter
  val gadtsConstr = noPrinter
  val hk = noPrinter
  val implicits = noPrinter
  val implicitsDetailed = noPrinter
  val lexical = noPrinter
  val init = noPrinter
  val inlining = noPrinter
  val interactiv = noPrinter
  val matchTypes = noPrinter
  val nullables = noPrinter
  val overload = noPrinter
  val patmatch = noPrinter
  val pickling = noPrinter
  val quotePickling = noPrinter
  val plugins = noPrinter
  val refcheck = noPrinter
  val simplify = noPrinter
  val staging = noPrinter
  val subtyping = noPrinter
  val tailrec = noPrinter
  val transforms = noPrinter
  val typr = noPrinter
  val unapp = noPrinter
  val variances = noPrinter

  val scopedGadt = new ScopePrinter
}
