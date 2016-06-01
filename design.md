The goal is to convert a RAML file into a corresponding Scala file. We proceed in phases.

Data.Yaml.Value (raw RAML file)
  Raml.Parser.parse
Raml.Parser.ParseTree (an algebraic representation of syntactically valid RAML files)
  Raml.Normalizer.normalize (fill in default values)
Raml.Normalizer.NormalizedTree
  Raml.Classifier.classify (propagate inherited values)
Raml.Classifier.ClassifiedTree (each type is classified as an Object, Union, Number or String)
  Raml.Analyzer.analyze (distinguish between tags and independent classes, and between Either-based unions and sealed-trait-based unions)
Raml.Analyzer.AnalyzedTree aka Raml.Raml

At this point we have a precise intermediate representation containing all the important
bits of the RAML file and none of the fluff. We're now ready to begin generating Scala
code from it. If we wanted to support other languages as well, the phases above would be
reused between languages but the phases below would not.

Raml.Raml
  Language.Scala.Convert
Language.Scala.ScalaTree RamlProps (Scala types annotated with the bits of Raml.Raml which haven't been converted yet)
  Language.Scala.Overlay.Pattern
  Language.Scala.Overlay.JsonFormat
  ...
Language.Scala.ScalaTree [Overlay] (Scala types annotated with each code-generating overlay)
  Language.Scala.Generator.generate
Language.Scala.Generator.GeneratedTree (a Scala AST, with methods and companion objects and everything)
  Language.Scala.Simplifier.simplify
Language.Scala.Simplifier.SimplifiedTree (a Scala AST, with case objects instead of case classes and without useless empty companion objects)
  Language.Scala.PrettyPrinter.prettyPrint
Data.CodeBlock
