# CSS::Specification::Compiler's t/compile.t matches rakudo

With its modules loading (#18153), CSS::Specification::Compiler's own
t/compile.t still died early. Bringing it to rakudo's output took these
general fixes:

- **Signatures.** A named part's trailing parens nested in a sub-signature
  (`:o(@)! (',', *%term)`) destructure its value, in the binder and in the
  dispatch matcher. An `@`/`%` sub-parameter binds the aggregate rather than
  the item container it arrived in (a hash element holds `$[...]`, which
  `for @args` iterated once), and one marked `is copy` owns a copy.
- **Dispatch.** A multi sub whose dispatch fails raises X::Multi::NoMatch
  instead of calling a same-named token of a grammar that encloses the
  caller's package (`multi sub ws` under the grammar `CSS::Specification`
  returned that grammar's `token ws` as a Regex).
- **`with` on attributes.** `with %!h<k> { } else { $_ = ... }` aliases the
  private attribute's element, as `given %!h<k>` does.
- **Declarations.** A `.=` declaration under a statement modifier
  (`my T $x .= new(...) if COND`) declares `$x` whatever the condition, as
  `=` already did; it was Nil.
- **RakuAST.** `RakuAST::Type::Simple.from-identifier(-parts)`, the
  `RakuAST::ArgList` type object as a call's absent argument list, and
  `.name` on `Var::Compiler::Routine` / `Var::Compiler::Distribution`.
