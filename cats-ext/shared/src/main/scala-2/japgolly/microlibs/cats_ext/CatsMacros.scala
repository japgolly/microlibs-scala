package japgolly.microlibs.cats_ext

import cats.Eq
import japgolly.microlibs.compiletime.MacroUtils
import scala.reflect.macros.blackbox

object CatsMacros {
  def deriveEq[A]: Eq[A] = macro CatsMacros.quietDeriveEq[A]
  def _deriveEq[A]: Eq[A] = macro CatsMacros.debugDeriveEq[A]
}


class CatsMacros(val c: blackbox.Context) extends MacroUtils {
  import c.universe._

  private val equal = c.typeOf[Eq[_]]

  def quietDeriveEq[T: c.WeakTypeTag]: c.Expr[Eq[T]] = implDeriveEq(false)
  def debugDeriveEq[T: c.WeakTypeTag]: c.Expr[Eq[T]] = implDeriveEq(true )
  def implDeriveEq[T: c.WeakTypeTag](debug: Boolean): c.Expr[Eq[T]] = {
    if (debug) println()
    val T = weakTypeOf[T]
    val t = T.typeSymbol

    def caseClass0: Tree =
      q"_root_.cats.Eq.instance[$T]((_, _) => true)"

    def caseClass1up(params: List[Symbol]): Tree = {
      val init = new Init("i$" + _)
      var cmps = Vector.empty[Tree]
      for (p <- params) {
        val (pn, pt) = nameAndType(T, p)
        val e = init.valImp(appliedType(equal, pt))
        cmps :+= q"$e.eqv(a.$pn,b.$pn)"
      }
      val expr = cmps.reduce((a, b) => q"$a && $b")
      q"""
        ..$init
        _root_.cats.Eq.instance[$T]((a, b) => $expr)
      """
    }

    def adt: Tree = {
      val init = new Init("i$" + _)
      val cases = crawlADT[CaseDef](T, (_, pt) => {
        val equalP = appliedType(equal, pt)
        tryInferImplicit(equalP).map { et =>
          val e = init.valDef(equalP, et)
          cq"x: $pt => b match {case y: $pt => $e.eqv(x,y); case _ => false}"
        }
      }, (_, pt) => {
        val u = appliedType(equal, pt)
        fail(s"Implicit not found: $u")
      })
      init wrap q"_root_.cats.Eq.instance[$T]((a,b) => a match {case ..$cases})"
    }

    val impl =
      if (t.isClass && t.asClass.isCaseClass) {
        ensureConcrete(T)
        val params = primaryConstructorParams(T)
        if (params.isEmpty)
          caseClass0
        else
          caseClass1up(params)
      } else
        adt

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[Eq[T]](impl)
  }
}