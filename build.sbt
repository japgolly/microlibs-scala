ThisBuild / organization  := "com.github.japgolly.microlibs"
ThisBuild / homepage      := Some(url("https://github.com/japgolly/microlibs-scala"))
ThisBuild / licenses      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0"))
ThisBuild / shellPrompt   := ((s: State) => Project.extract(s).currentRef.project + "> ")
ThisBuild / versionScheme := Some("early-semver")

val root           = Microlibs.root
val rootJVM        = Microlibs.rootJVM
val rootJS         = Microlibs.rootJS

val adtMacrosJVM   = Microlibs.adtMacrosJVM
val adtMacrosJS    = Microlibs.adtMacrosJS
val catsExtJVM     = Microlibs.catsExtJVM
val catsExtJS      = Microlibs.catsExtJS
val compileTimeJVM = Microlibs.compileTimeJVM
val compileTimeJS  = Microlibs.compileTimeJS
val disjunctionJVM = Microlibs.disjunctionJVM
val disjunctionJS  = Microlibs.disjunctionJS
val multimapJVM    = Microlibs.multimapJVM
val multimapJS     = Microlibs.multimapJS
val nameFnJVM      = Microlibs.nameFnJVM
val nameFnJS       = Microlibs.nameFnJS
val nonemptyJVM    = Microlibs.nonemptyJVM
val nonemptyJS     = Microlibs.nonemptyJS
val recursionJVM   = Microlibs.recursionJVM
val recursionJS    = Microlibs.recursionJS
val stdlibExtJVM   = Microlibs.stdlibExtJVM
val stdlibExtJS    = Microlibs.stdlibExtJS
val testUtilJVM    = Microlibs.testUtilJVM
val testUtilJS     = Microlibs.testUtilJS
val typesJVM       = Microlibs.typesJVM
val typesJS        = Microlibs.typesJS
val utilsJVM       = Microlibs.utilsJVM
val utilsJS        = Microlibs.utilsJS

val bench          = Microlibs.bench
