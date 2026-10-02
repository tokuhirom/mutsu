# The compiler's body scans walk the typed AST visitor

The questions the compiler asks about a block or routine body before it emits
code — does it need a `let`/`temp` save frame, a `state` reset, a succeed
barrier, a per-iteration topic scope, a block-local scope; does it use
`return-rw` or return a value against a definite return type; can the VM
compile it on the fly or run a `map` block natively; does it read `@_`/`%_` —
were hand-rolled recursive walkers, each with a `_ =>` arm that skipped every
expression form it had not listed. They now implement the typed visitor
(ADR-0137). Where a scan must stop (a closure, a nested routine, a loop body
that owns its own frame, a block with its own scope) is stated once in
`compiler/scope_scan.rs` or as an explicit hook arm; the LSP outline
(`analysis/symbols.rs`) moved too. 32 of the 71 walkers in these files are
gone; the rest are code generation, value-path spines and shape classifiers,
annotated in the ratchet baseline.

Reaching every position fixed real wrong answers, each checked against `raku`:

- `temp`/`let` hidden in a ternary arm, an operand, a list element or a
  `given`/`when` body was never restored (`{ 1 ?? (temp $a = 5) !! 0 }` left
  `$a` at 5).
- `state $n = 0 if 1` inside an `if` block did not restart with its block
  (`f()` counted 1, 2, 3 where raku prints 1, 1, 1).
- `1 and return-rw $x` did not hand back the container (`g() = 5` died).
- `1 and return 5` in a `sub (--> 42)` was accepted; rakudo rejects it with
  "No return arguments allowed".

The heredoc scope check now also reaches a heredoc behind a statement
modifier, which exposed a parser heuristic: a heredoc marker line counted as
closing a block whenever any `}` followed it, so the subscripts in
`X.new(:note(qq:to/W/)).throw unless %h{$k}{$v};` raised a bogus "Variable is
not declared". Only an unbalanced `}` closes a block now.

A sub or class declared in a nested block or a closure now shows up in the
outline, and `has_block_placeholders` asks the same placeholder collector the
closure compiler uses, so the two answers can no longer disagree.
