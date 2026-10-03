# RakuAST: CompUnit, in-place builders, dynamic declarations, regex `:i`/`:m`

`Needle::Compile` turns a search spec into a matcher by building and rewriting
RakuAST, then `EVAL`ing it. All four of its test files now produce output
byte-identical to rakudo's. Getting there needed:

## RakuAST model and lowering

- `Str.AST(:compunit)` returns a `RakuAST::CompUnit` with:
  - `statement-list`;
  - a fresh 40-hex-digit `comp-unit-name`;
  - `replace-statement-list`.

  `EVAL` lowers it, and `RakuAST::CompUnit.new` builds one.
- New in-place mutators, which change the node shared by every holder, as in
  rakudo:
  - `StatementList.unshift-statement`;
  - `Statement::Expression.set-expression`;
  - `ArgList.push`.
- `my $*x := $_` round-trips:
  - the converter renders the `*` twigil (it used to refuse dynamic
    declarations);
  - `VarDeclaration::Simple.new` accepts that twigil and an
    `Initializer::Bind`;
  - lowering declares a dynamic variable.
- Regex internal modifiers now have their own nodes,
  `RakuAST::Regex::InternalModifier::IgnoreCase` / `IgnoreMark`. They cover
  `:i`, `:ignorecase`, `:m`, `:ignoremark` and the negated `:!i`. The modifier
  keeps its spelling and is never wrapped in `WithWhitespace`, and a `^` after
  a leading modifier is still the start anchor.
- Lowering sees through a role mixed into a node
  (`$ast but Type<and>`).
- `Term::Name.new(Name.from-identifier("False"))` evaluates to `Bool::False`
  rather than the string `"False"`.

## Mixin type objects

- `constant StrType = Str but Type` names the composed type `Str+{Type}`. As a
  parameter constraint and in smartmatch, a value now matches it only when it
  is a `Str` that also does `Type`. A plain `"bar" ~~ StrType` was `True`, and
  the mixed value was rejected by `sub f(StrType:D $x)`.
- In multi dispatch the composed type ranks narrower than `Str`.

## Smaller fixes

- A `Seq` argument picks an `@` candidate over an `Any:D` one, as in rakudo
  (`handle($spec.words, %_)`).
- `"&code.name()"` interpolates a method call on the code object.
- A container's `.raku` uses a mixed-in role's own `raku` method for that
  element, e.g. `("foo" but T<a>,).raku`.
