import Dependencies._

ThisBuild / scalaVersion     := "3.3.6"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "fr.esgi"
ThisBuild / organizationName := "esgi"
// Scalafmt
ThisBuild / scalafmtOnCompile := true

run / fork := true
run / connectInput := true
Compile / mainClass := Some("fr.esgi.al.funprog.Main")

lazy val root = (project in file("."))
  .settings(
    name := "funprog-AL",
    scalacOptions ++= compilerOptions,
    libraryDependencies ++= Seq(
      betterFiles,
      munit % Test,
      scalatic
    ),
    // Wartremover
    wartremoverWarnings ++= Warts.all,
    wartremoverErrors ++= Warts.all
  )


val compilerOptions = Seq(
  // Common settings
  //
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-explain-types", // Explain type errors in more detail.
  "-explain",
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Ysafe-init", // Wrap field accessors to throw an exception on uninitialized access.
  "-Werror", // Fail the compilation if there are any warnings.
  "-Ycheck-all-patmat",
  "-Ycheck-reentrant",
  "-Yexplicit-nulls",
  // Warning settings
  //
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Wunused:imports,privates,locals,explicits,implicits,params,linted",
  // // Linting
  // //
  // "-Xlint:adapted-args",
  // "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  // "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  // "-Xlint:missing-interpolator",
  // "-Xlint:doc-detached",
  // "-Xlint:private-shadow",
  // "-Xlint:type-parameter-shadow",
  // "-Xlint:poly-implicit-overload",
  // "-Xlint:option-implicit",
  // "-Xlint:package-object-classes",
  // "-Xlint:stars-align",
  // "-Xlint:constant",
  // "-Xlint:nonlocal-return",
  // "-Xlint:valpattern",
  // "-Xlint:eta-zero,eta-sam",
  // "-Xlint:deprecation",
  // "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  // "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
  // "-Xfatal-warnings"
)

val consoleOptions = Seq()
