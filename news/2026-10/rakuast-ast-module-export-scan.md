# `Str.AST` scans imported module exports

Plain `Str.AST` now parses with the interpreter's module search paths, so
exported names from imported modules are available to the RakuAST converter.
The same applies to `Str.AST(:compunit)`.
