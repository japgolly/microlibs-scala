package japgolly.microlibs.compiletime

object Scala3CompilationTests:

  class X
  InlineUtils.printCode(new X)
  InlineUtils.printTasty(new X)
