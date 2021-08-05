package japgolly.microlibs.compiletime

import scala.quoted.*

object NewInstance {
  import MacroEnv.*

  type FailFn       = () => Nothing
  type TermLookupFn = (q: Quotes) ?=> (q.reflect.ValDef , FailFn) => q.reflect.Term
  type TypeLookupFn = (q: Quotes) ?=> (q.reflect.TypeDef, FailFn) => q.reflect.TypeTree

  def of[A: Type](findTermArg          : Option[TermLookupFn] = None,
                  findTypeArg          : Option[TypeLookupFn] = None,
                  autoPopulateImplicits: Boolean              = true,
                )(using Quotes): Expr[A] = {

    import quotes.reflect.*

    val A = TypeRepr.of[A].dealias.typeSymbol

    if A.flags.is(Flags.Abstract) then
      fail(s"${Type.show[A]} is abstract. It needs to be a concrete.")

    if A.flags.is(Flags.Trait) then
      fail(s"${Type.show[A]} is a trait. It needs to be a class.")

    val ctor = A.primaryConstructor

    def generateTypeArg(d: TypeDef): TypeTree = {
      val failFn: FailFn = () => fail(s"Don't know how to populate the type parameter ${d.name} in new ${Type.show[A]}[...]")
      findTypeArg match {
        case Some(f) => f(d, failFn)
        case None    => failFn()
      }
    }

    def generateTermArg(d: ValDef, isImplicit: Boolean): Term = {
      val failFn: FailFn = () => {
        if autoPopulateImplicits && isImplicit then {
          println()
          println()
          d.tpt.summonOrError
          println()
          println()
        }
        val pre = if isImplicit then "implicit " else ""
        fail(s"Don't know how to populate the parameter ($pre${d.name}: ${d.tpt.show}) in new ${Type.show[A]}(...)")
      }

      var result = Option.empty[Term]

      if autoPopulateImplicits && isImplicit then
        for (e <- d.tpt.summon)
          result = Some(e.asTerm)

      if result.isEmpty then
        for (f <- findTermArg)
          result = Some(f(d, failFn))

      result getOrElse failFn()
    }

    var classType = TypeRepr.of[A].dealias.asTypeTree
    var typeArgs  = Vector.empty[TypeTree]
    var termArgs  = List.empty[List[Term]]

    // Extract provided class types
    classType.tpe match {

      // A = F[X, Y, ..]
      case AppliedType(cls, args) =>
        classType = cls.asTypeTree
        typeArgs  = args.iterator.map(_.asTypeTree).toVector

      case _ =>
    }

    // Extract args
    locally {
      var typeParamsToSkip = typeArgs.length

      def generateArgs(clauses: List[ParamClause]): Unit =
        clauses.foreach {
          case c: TermParamClause =>
            val isImplicit = c.isImplicit || c.isGiven
            termArgs = termArgs ::: c.params.map(generateTermArg(_, isImplicit = isImplicit)) :: Nil
          case c: TypeParamClause =>
            for (p <- c.params)
              if typeParamsToSkip > 0
              then typeParamsToSkip -= 1
              else typeArgs :+= generateTypeArg(p)
        }

      ctor.tree match {
        case DefDef(_, p, _, _) => generateArgs(p)
        case t                  => fail(s"Don't know how to interpret the constructor of ${Type.show[A]}\n$t")
      }
    }

    // Post-process args
    val typeArgList = typeArgs.toList
    typeArgs = null
    locally {
      if typeArgList.nonEmpty then
        classType = Applied(classType, typeArgList)

      if termArgs.isEmpty then
        termArgs = Nil :: Nil // `new X` is translated to `new X()`
    }

    // Build AST
    val result: Term = {
      var ast: Term =
        Select(New(classType), ctor)

      if typeArgList.nonEmpty then
        ast = TypeApply(ast, typeArgList)

      for (args <- termArgs)
        ast = Apply(ast, args)

      ast
    }

    result.asExprOf[A]
  }
}
