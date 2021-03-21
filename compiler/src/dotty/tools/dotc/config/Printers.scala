package dotty.tools.dotc.config

import scala.util.DynamicVariable

object Printers {

  class Printer {
    def println(msg: => String): Unit = System.out.println(msg)

    def force[S](body: => S): S = ???
  }

  object noPrinter extends Printer {
    inline override def println(msg: => String): Unit = ()

    inline override def force[S](body: => S): S = ???
  }

  class SwitchedPrinter(defaultOn: Boolean = false) extends Printer {
    val switch = DynamicVariable(defaultOn)

    override def println(msg: => String): Unit =
      if switch.value then
        System.out.println(msg)
      else
        ()

    override def force[S](body: => S): S = {
      switch.withValue(true) { body }
    }
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
  val gadts = new SwitchedPrinter(defaultOn = false)
  val gadtsConstr = new SwitchedPrinter(defaultOn = false)
  val hk = noPrinter
  val implicits = noPrinter
  val implicitsDetailed = noPrinter
  val lexical = noPrinter
  val init = noPrinter
  val inlining = noPrinter
  val interactiv = noPrinter
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
}
