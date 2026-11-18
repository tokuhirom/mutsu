# CSS::Specification::Compiler loads, and its suite matches rakudo

CSS::Specification::Compiler builds CSS property grammars as RakuAST trees.
Its modules did not load under mutsu (`blocked_load`), and getting both of its
test files to match rakudo took fixes to the parser, the binder, dispatch and
the RakuAST node model, each a general gap:

- **Parsing.** `$x .= &f: args` (the colon-argument form of a mutating call
  through a code object) parses, and a leading topic call `.&lit'r` reads the
  whole identifier, apostrophe included. A `for ( LIST ) { ... }` whose list
  holds a `#` inside a quoting construct the C-style probe does not know
  (`q<[<a> | <b> ]#>`) is no longer misread as a C-style `for`: the probe's
  hit is confirmed against the parenthesized group the term parser reads.
- **Declarations.** `my RakuAST::Name $n` declares (a RakuAST node type was
  rejected as undeclared in a `my`, though signatures accepted it), and a `.=`
  declaration under a statement modifier (`my T $x .= new(...) if COND`)
  declares `$x` whatever the condition, as `=` already did.
- **Signatures.** A List element that is a Pair held in an item container
  (`('?', $pair)`) is a named part of the list's Capture, for binding and for
  dispatch. A named part's trailing parens nested in a sub-signature
  (`:o(@)! (',', *%term)`) destructure its value. An `@`/`%` sub-parameter
  binds the aggregate rather than the item container it arrived in, and one
  marked `is copy` owns a copy. A routine whose signature declares `$_` itself
  (`sub f(Str:D() $_)`) keeps the coerced value when called through `.map(&f)`.
- **Dispatch.** A multi sub whose dispatch fails raises X::Multi::NoMatch
  instead of calling a same-named token of a grammar that encloses the
  caller's package (`multi sub ws` under the grammar `CSS::Specification`).
- **`with` on attributes.** `with %!h<k> { } else { $_ = ... }` aliases the
  private attribute's element, as `given %!h<k>` does.
- **Classes.** An `also is Parent` whose name is so far only the package stub
  a nested unit left (`unit module A::B::C::U` stubs `A::B`) is resolved after
  the class body, whose own `use` loads the real grammar.
- **RakuAST.** `RakuAST::Type::Simple.from-identifier(-parts)`, the
  `RakuAST::ArgList` type object as a call's absent argument list, and `.name`
  on `Var::Compiler::Routine` / `Var::Compiler::Distribution`.
