# Eight Interpreter side channels become parameters, and three leaks close

ADR-10779 D3 removes the `handoff` fields of `Interpreter` one at a time. These fields are
values a caller parks on the interpreter for a callee or a later opcode to pick up. The
first batch takes the struct from 102 to 94 direct fields. In each case the replacement
follows the data flow the field was standing in for:

- A subset's `where` predicate that fails by throwing (`fail "msg"`) is now returned along
  the type check that ran it, through `type_matches_value_why`. As a field it outlived its
  check: after `try { 0 ~~ S }`, an unrelated `my Str $s = 3` died with S's custom message
  instead of the type-check error.
- The role initializer on the right of `does`/`but` (`$x does R(v)`) is now a compile-time
  shape, as in Rakudo. Only a top-level call with exactly one argument (positional, or named
  with its name ignored) is an initializer. The old flag covered the whole right operand and stayed set when the operand
  died, so a later plain `R(5)` returned a Pair instead of throwing `X::Coerce::Impossible`.
  `R(1, 2)` is now a coercion error, and a role's `CALL-ME` is no longer consulted for an
  initializer, both as in raku.
- EVAL gets its unit's free-variable writes through the out-parameter the carrier evaluator
  already had, so it no longer needs an append-only log with a mark.
- A value call (`$b($v)`) hands the block a `TopicArgSite` (is the argument container-less,
  and which caller variable is behind it) instead of two fields that `push_call_frame` had
  to clear.
- The shaped-declaration mark joins the packed mark-context word it always behaved like, so
  the call-boundary guard now isolates it too.
- Two memos that were misfiled as side channels move into `ResolutionCaches`.
