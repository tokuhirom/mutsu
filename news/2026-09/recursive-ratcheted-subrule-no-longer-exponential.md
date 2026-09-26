# Recursive ratcheted subrules no longer take exponential time

`token A { '{' [ <A> | . ]*? '}' }` parsed `{` ~ `'{ab{c}d}' x $n` ~ `}` in
2 seconds at `n = 6` and did not finish at `n = 8`; Rakudo takes 3 ms at any
size. The same shape is `ANTLR4::Grammar`'s `ACTION` token, which never
finished the `@members` block of `ANTLRv4Lexer.g4`. Two independent causes
compounded:

- **Every end of a recursive rule was enumerated.** A ratcheted caller can
  only use the first end of a `<subrule>`, and ADR-0073 walks the callee with
  `first_only` for that reason — but only when the callee's body calls no
  rule at all, because a cut-short walk could hide a left recursion from the
  growing-seed loop. Every interior rule of a real grammar calls rules, so it
  enumerated its whole end set, and each end enumerated the nested ones. The
  new `regex_left_call_graph` follows only the calls a rule can make before
  it consumes anything and proves when its own name cannot come back at the
  same position — the only case the guard exists for. Such rules now take
  `first_only` too. With `||` in place of `|` this alone makes the shape flat.
- **LTM ranking re-measured the same thing.** Ranking a `|` measures each
  branch by walking it in declarative mode. Inside that walk, every nested
  `|` measured its branches again and every nested `<A>` was walked again,
  once per path that reached it. `regex_ltm_memo` now answers a repeated
  `(branch, position)` measurement and a repeated `<subrule>` call inside one
  outermost measurement from a memo. No user code runs during a measurement,
  so the key is the pattern (or subrule spec) identity, position, package,
  token generation, and an epoch that moves whenever the engine installs or
  removes `$*` parameters or a regex closure scope. A walk that consulted
  left-recursion state is never stored.

`n = 32` now takes 0.05 s. Pinned by
`t/regex/syntax/recursive-ratchet-subrule-first-end.t`, which also counts how
often an embedded block fires: once per `<A>` entered, as in Rakudo, not once
per end computed.

Closes #9579.
