The goal is to convert a RAML file into a corresponding Scala file. We proceed in phases.

Data.Yaml.Value (raw RAML)
  Parser.parse
Parser.ParseTree (an algebraic representation of syntactically valid RAML files)
  Normalizer.normalize (fill in default values)
Normalizer.NormalizedTree
  Classifier.classify (propagate inherited values)
Classifier.ClassifiedTree (each type is classified as an Object, Union, Number or String)
  Analyzer.analyze (distinguish between tags and independent classes, and between Either-based unions and sealed-trait-based unions)
Analyzer.AnalyzedTree
  Generator.generate
Generator.GeneratedTree (a Scala AST, with methods and companion objects and everything)
  Simplifier.simplify
Simplifier.SimplifiedTree (a Scala AST, with case objects instead of case classes and without useless empty companion objects)
  PrettyPrinter.prettyPrint
IndentedCode
