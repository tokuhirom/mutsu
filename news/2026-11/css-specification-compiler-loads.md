# CSS::Specification::Compiler loads, and six general fixes behind it

CSS::Specification::Compiler builds grammars as RakuAST trees. It was
`blocked_load` in the ecosystem ledger. Getting it to load and through the
first part of its `t/compile.t` exposed six general gaps:

- **`$x .= &f: args`.** A mutating call of a code object now takes colon
  arguments, as one after a method name does.
- **`.&lit'r`.** The topic form of a code call reads the callee name with the
  identifier rules, so apostrophe joints are allowed.
- **`also is P` from a precompiled module.** `grammar P::Ext { use P; also is P }`,
  loaded from the precompilation cache, runs the body's `use P` only when the
  body runs. When an earlier `P::Util` had already declared `P` as a stub
  package, the class header rejected `P` as a package that cannot be
  inherited from. The body-level `also is` parent is now deferred to the body
  before the package check, and the deferred resolution raises that same
  `X::Inheritance::Unsupported` if the body leaves `P` a package.
- **C-style `for` detection.** The obsolete C-style `for (;;)` header is
  recognised with the expression parser as `( EXPR? ; EXPR? ; EXPR? )`, as
  rakudo does. A `#` inside `q<[...]#>` no longer reads as a comment, and
  `for (1; 2) { }` is a semicolon list again. The error message is rakudo's.
- **Containered Pairs in a destructured List.** Destructuring a List takes a
  Pair the list holds in a variable's container (`('?', $p)`) as a named
  argument, as `List.Capture` does.
- **Coercive `$_` parameters.** A routine with an explicit coercive topic
  parameter (`sub f(Str:D() $_)`) keeps the coerced value when it is called
  through its code object or as a `.map` callback.
- **RakuAST types on variables.** `my RakuAST::Name $n` declares a variable
  constrained by a RakuAST node class.

The rest of `t/compile.t` is waiting on #18149: `.map(&ws)` over the module's
own `proto sub ws` reaches a loaded grammar's `token ws`.
