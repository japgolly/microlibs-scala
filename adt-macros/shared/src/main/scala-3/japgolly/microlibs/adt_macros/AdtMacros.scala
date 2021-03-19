package japgolly.microlibs.adt_macros

import japgolly.microlibs.macro_utils.MacroUtils
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.univeq.UnivEq
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

object AdtMacros:
  import MacroUtils.Ops.*
  import MacroUtils.{logAll, fail}

  private def nonEmptyVector[A: Type](exprs: Seq[Expr[A]])(using Quotes): Expr[NonEmptyVector[A]] =
    val head = exprs.head
    val tail = Varargs(exprs.tail)
    '{ NonEmptyVector[A]($head, $tail*) }

  // ===================================================================================================================

  inline def adtValues[T]: NonEmptyVector[T] =
    ${ adtValuesImpl[T](false) }

  inline def _adtValues[T]: NonEmptyVector[T] =
    ${ adtValuesImpl[T](true) }

  private def adtValuesImpl[A: Type](debug: Boolean)(using Quotes): Expr[NonEmptyVector[A]] =
    MacroUtils.withNonEmptySumTypeTypes(Type.of[A])([types] => (_: Type[types]) ?=> {
      def values[T: Type]: List[Expr[A]] =
        Type.of[T] match
          case '[h *: t] =>
            val e = MacroUtils.exprSingletonOrThrow[h]
            e.asInstanceOf[Expr[A]] :: values[t]
          case '[EmptyTuple] =>
            Nil
      val all    = values[types]
      val result = nonEmptyVector[A](all)
      if debug then println(s"\n${result.show}\n")
      result
    })

  // ===================================================================================================================

  /** Because sometimes order matters. */
  inline def adtValuesManually[A](inline values: A*): NonEmptyVector[A] =
    ${ adtValuesManuallyImpl[A]('values, false) }

  inline def _adtValuesManually[A](inline values: A*): NonEmptyVector[A] =
    ${ adtValuesManuallyImpl[A]('values, true) }

  private def adtValuesManuallyImpl[A: Type](varargs: Expr[Seq[A]], debug: Boolean)(using Quotes): Expr[NonEmptyVector[A]] =
    MacroUtils.withNonEmptySumTypeTypes(Type.of[A])([types] => (_: Type[types]) ?=> {
      import quotes.reflect.*

      var map = MacroUtils.mapByFieldTypes[types, Option[Expr[A]]]([t] => (_: Type[t]) ?=> None)
      val keyList = map.keys.toList

      // TODO ensure all singletons

      val Varargs(argList) = varargs
      for (arg <- argList)
        val argType = arg.asTerm.tpe
        keyList.filter(argType <:< _) match
          case t :: Nil =>
            map(t) match
              case None    => map = map.updated(t, Some(arg))
              case Some(a) => fail(s"Duplicate arguments to ${t.show} provided:\n  - ${a.show}\n  - ${arg.show}")
          case a :: b :: _ =>
            fail(s"(${arg.show}: $argType) is a subtype of both ${a.show} and ${b.show}")
          case Nil =>
            fail(s"(${arg.show}: $argType) is not known to be a subtype of ${Type.show[A]}")

      map.foreach {
        case (_, Some(_)) => ()
        case (t, None)    => fail(s"An instance of ${t.show} not provided.")
      }

      val result = nonEmptyVector[A](argList)
      if debug then println(s"\n${result.show}\n")
      result
    })

// ===================================================================================================================

  type AdtIso[Adt, A] = (Adt => A, A => Adt, NonEmptyVector[Adt], NonEmptyVector[A])

  inline def adtIso[Adt, A](inline f: Adt => A): AdtIso[Adt, A] =
    ${ adtIsoImpl[Adt, A]('f, false) }

  inline def _adtIso[Adt, A](inline f: Adt => A): AdtIso[Adt, A] =
    ${ adtIsoImpl[Adt, A]('f, true) }

  private def adtIsoImpl[Adt: Type, A: Type](trans: Expr[Adt => A], debug: Boolean)(using Quotes): Expr[AdtIso[Adt, A]] =
    MacroUtils.withNonEmptySumTypeTypes(Type.of[Adt])([types] => (_: Type[types]) ?=> {
      import quotes.reflect.*

      val fromFn    = MacroUtils.extractInlineAdtMappingFn(trans)
      var toCases   = Vector.empty[CaseDef]
      var toValues  = Set.empty[Any]
      var adtValues = Seq.empty[Expr[Adt]]

      // if debug then logAll("Mappings", fromFn)(identity)

      for (adt <- MacroUtils.setOfFieldTypes[types])

  //       ensureConcrete(adt)
  //       if (primaryConstructorParams(adt).nonEmpty)
  //         fail(s"$adt requires constructor params.")

        val matchingCases = fromFn.filter(adt <:< _._1.fold(_.tpe, _.tpe))
        if (matchingCases.size != 1)
          fail(s"Found ${matchingCases.size} cases for ${adt.show}.")

        val fromCase = matchingCases.head
        val toValue = fromCase._2.asConstant.value
        if (toValues contains toValue)
          fail(s"Non-unique value encountered: $toValue")
        toValues += toValue

        val adtType = adt.asType.asInstanceOf[Type[Adt]]
        val adtObj = MacroUtils.exprSingletonOrThrow(using adtType)
        adtValues :+= adtObj
        toCases :+= CaseDef(fromCase._2, None, adtObj.asTerm)
      end for

      // logAll("toCases", toCases)(identity)
      // logAll("toValues", toValues)(identity)
      // logAll("adtValues", adtValues)(_.show)

      val toFn = MacroUtils.anonymousMatch[A, Adt](toCases)

      val result = '{
        val from: Adt => A = $trans
        val to: A => Adt = $toFn
        val adts = ${ nonEmptyVector[Adt](adtValues) }
        val tos = ${ nonEmptyVector[A](fromFn.map(_._2.asExprOf[A])) }
        assert(adts.length == tos.length)
        (from, to, adts, tos)
      }

      if debug then println(s"\n${result.show}\n")
      result
    })

  // ===================================================================================================================

  type AdtIsoSet[Adt, T] = (Adt => T, T => Adt, NonEmptySet[Adt], NonEmptySet[T])

  inline def adtIsoSet[Adt, A](inline f: Adt => A)(using inline ueAdt: UnivEq[Adt], inline ueA: UnivEq[A]): AdtIsoSet[Adt, A] =
    ${ adtIsoSetImpl[Adt, A]('f, 'ueAdt, 'ueA, false) }

  inline def _adtIsoSet[Adt, A](inline f: Adt => A)(using inline ueAdt: UnivEq[Adt], inline ueA: UnivEq[A]): AdtIsoSet[Adt, A] =
    ${ adtIsoSetImpl[Adt, A]('f, 'ueAdt, 'ueA, true) }

  private def adtIsoSetImpl[Adt: Type, A: Type](trans: Expr[Adt => A],
                                                ueAdt: Expr[UnivEq[Adt]],
                                                ueA: Expr[UnivEq[A]],
                                                debug: Boolean)
                                               (using Quotes): Expr[AdtIsoSet[Adt, A]] =
    val nevImpl = adtIsoImpl[Adt, A](trans, debug = false)
    val result = '{
      val (from,to,adtVec,toVec) = $nevImpl
      val adtSet = adtVec.toNES[Adt](using $ueAdt)
      val toSet = toVec.toNES[A](using $ueA)
      assert(adtSet.forall(a => to(from(a)) == a))
      (from, to, adtSet, toSet)
    }
    if debug then println(s"\n${result.show}\n")
    result

  // ===================================================================================================================

  inline def  valuesForAdt[Adt, A](inline f: Adt => A): NonEmptyVector[A] =
    ${ valuesForAdtImpl[Adt, A]('f, false) }

  inline def  _valuesForAdt[Adt, A](inline f: Adt => A): NonEmptyVector[A] =
    ${ valuesForAdtImpl[Adt, A]('f, true) }

  private def valuesForAdtImpl[Adt: Type, A: Type](trans: Expr[Adt => A], debug: Boolean)(using Quotes): Expr[NonEmptyVector[A]] =
    MacroUtils.withNonEmptySumTypeTypes(Type.of[Adt])([types] => (_: Type[types]) ?=> {
      import quotes.reflect.*

      val valueFn = MacroUtils.extractInlineAdtMappingFn(trans)
      val values  = valueFn.map(_._2)
      val types   = MacroUtils.setOfFieldTypes[types]

      var unseen = types.toSet
      for ((_case, ind) <- valueFn.iterator.map(_._1).zipWithIndex) {
        val _type = _case.fold(_.tpe, _.tpe)
        val matches = unseen.filter(_ <:< _type)
        if (matches.isEmpty)
          fail(s"Case ${ind + 1} (${_type}) doesn't match any remaining cases (${MacroUtils.showUnorderedTypes(unseen)}).")
        else
          unseen --= matches
      }
      if (unseen.nonEmpty)
        fail(s"The following types are unaccounted for: ${MacroUtils.showUnorderedTypes(unseen)}")

      val result = nonEmptyVector[A](values.map(_.asExprOf[A]))
      if debug then println(s"\n${result.show}\n")
      result
    })

  // ===================================================================================================================

  inline def valuesForAdtF[Adt, A](inline f: Adt => A): (NonEmptyVector[A], Adt => A) =
    ${ valuesForAdtFImpl[Adt, A]('f, false) }

  inline def _valuesForAdtF[Adt, A](inline f: Adt => A): (NonEmptyVector[A], Adt => A) =
    ${ valuesForAdtFImpl[Adt, A]('f, true) }

  private def valuesForAdtFImpl[Adt: Type, A: Type](f: Expr[Adt => A], debug: Boolean)(using Quotes): Expr[(NonEmptyVector[A], Adt => A)] =
    val nev = valuesForAdtImpl[Adt, A](f, debug = false)
    val result = '{ ($nev, $f) }
    if debug then println(s"\n${result.show}\n")
    result
