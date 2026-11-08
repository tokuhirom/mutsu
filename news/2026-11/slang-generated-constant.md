# A slang production can install a constant for the importing unit

`$*R.current-scope.merge-generated-lexical-declaration(RakuAST::VarDeclaration::Implicit::Constant.new(:name, :value, :scope))`
inside a slang production now declares `name` as a constant of the unit that imports the slang,
bound to the value (a production that attaches a one-statement `StatementList` stands for that
statement). `RakuAST::VarDeclaration::Implicit::Constant` exists with `name`/`scope`/`value`.
A class value is not usable yet: it lives in the slang host's registry (ADR-13370, Proposed).
Covered by `t/lang/parsing/slang-generated-constant.t` (identical under `raku`).
