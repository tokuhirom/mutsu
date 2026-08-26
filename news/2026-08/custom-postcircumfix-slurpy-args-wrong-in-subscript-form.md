# A custom `postcircumfix:<[- ]>` subscript now beats the built-in one and passes one list argument

The `Language/faq.rakudoc` example

```raku
multi postcircumfix:<[- ]> (Str:D $str is copy, +@indices) { ... }
say '0123456789'[- 1..3, 8 ];
```

prints `045679` in rakudo. mutsu printed a `Failure` about out-of-range indices.

## Root cause — three independent bugs

1. **The custom operator never ran.** The declared-postcircumfix check sat *after*
   the built-in `(...)`/`[...]`/`{...}` subscripts in the postfix loop, so
   `[- 1..3, 8 ]` was consumed as an ordinary index with the operand `[-1..3, 8]`
   (`--dump-ast` showed a plain `Expr::Index`) and the operator was never
   consulted. Rakudo resolves this by longest token: the declared two-character
   opener `[-` beats the built-in one-character `[`.

2. **The bracket contents were parsed as one `expression()`,** which stops at the
   comma — so everything after `1..3` was silently dropped. Probing rakudo with a
   `|c` capture parameter settles the shape exactly: `'abc'[- 1, 2]` is
   `\("abc", (1, 2))` and `'abc'[- 8]` is `\("abc", 8)`. The bracket holds **one**
   argument parsed at comma (list) precedence, not a positional argument list —
   which is precisely what makes the operator's `+@indices` single-argument-rule
   slurpy see `[1..3, 8]`.

3. **`multi` dispatch rejected every `+@` candidate.** mutsu's parser records
   `+@a` as a plain `@` parameter carrying an `onearg` flag rather than setting
   `slurpy`, and the candidate matcher asked only about `slurpy`. So
   `multi f($s, +@i)` looked like a fixed two-argument candidate taking a
   `Positional`, and `f("x", 8)` / `f("x", 8, 9)` found no candidate at all —
   while the identical **non**-`multi` sub bound fine, because the binder itself
   already implements `+@` correctly.

## Fix

The postcircumfix handler moved ahead of the built-in subscripts in
`src/parser/expr/postfix/loop_.rs`, gated by the longest-token rule (a
one-character opener that *is* a built-in subscript opener ties, and the built-in
keeps it), and it now parses its bracket with `parse_comma_or_expr`. A new
`ParamDef::is_variadic()` (`slurpy || double_slurpy || onearg`) replaces the bare
`slurpy` test everywhere arity is being reasoned about — the candidate filters and
optional-positional count in `src/runtime/dispatch_resolve.rs` and
`src/runtime/dispatch_candidates.rs`, and the arity/type checks in
`src/runtime/types/args_matching.rs`.

The doc example now prints `045679`, and `multi f($s, +@i)` accepts one, two or
three arguments exactly as rakudo does. Pinned by
`t/custom-operator-and-term-parsing.t` section 4.
