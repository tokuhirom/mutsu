# Destructive s/// against accessor and element LHS writes back (Text::CSV runtime sweep round 5)

`$x ~~ s///` had a writeback path only for a plain variable LHS: the
`SmartMatchExpr` opcode carried `lhs_var`/`lhs_slot` and re-wrote the named
variable after a destructive substitution. Any other lvalue shape silently
dropped the modification — `$f.text ~~ s{ <[\ \t]>+ $ } = ""` (an `is rw`
accessor), `@ch[$i + 1] ~~ s{^ "0"} = ""` (an array element), and
`%h<k> ~~ s///` (a hash element) all matched, mutated the topic copy, and
threw it away. In Raku all three topicalize a *container*, so the
substitution's assignment lands in the attribute / element.

Both shapes were found in Text::CSV's `65_allow.t` (github.com/Tux/CSV):
`allow_whitespace` trims trailing blanks before a separator via the accessor
form (tests 171–417: `1 , foo , bar` parsed but kept `"1 "`, `"foo "`), and
the `allow_unquoted_escape` NUL escape `=0` strips its `"0"` chunk via the
element form (test 1022: `"\0"` came out as `"\0" ~ "0"`).

Two mechanisms, chosen by LHS shape at compile time:

- **Accessor LHS** (`$obj.meth ~~ s///`, zero-arg method on a variable):
  `SmartMatchExpr`'s writeback metadata became `Option<Box<SmartMatchLhs>>`
  (a `Var`/`Method` enum — boxing keeps `OpCode` at 48 bytes). After a
  destructive RHS actually modified the topic, the VM re-invokes the accessor
  as an lvalue via `assign_method_lvalue_with_values` — the same runtime path
  `$obj.meth = $v` compiles to, so a non-rw accessor raises the same error a
  plain assignment would. The enclosing scope's readonly `_` (e.g. a `for`
  loop topic) no longer blocks the substitution, since the target is the
  accessor's container, not the surrounding topic.
- **Element LHS** (`@a[IDX] ~~ s///` / `%h<KEY> ~~ s///`, simple variable
  target, simple scalar index): desugared entirely in the compiler — the
  index is evaluated once into a hidden local, the element value flows
  through the existing `Var` writeback into a hidden value local, and an
  identity-gated `IndexAssign` stores it back only when the topic actually
  changed (a non-matching attempt stays store-free, so immutable elements
  and custom `ASSIGN-POS` never observe a spurious store). No VM changes.

Also fixed in the same round, a regression from round 4's listop-argument
precedence change (caught by CI on `S02-types/whatever.t` and
`S32-str/split.t`): the listop argument parser applies `expression`'s
finished-expression wrap — including the `try_wrap_whatevercode_call_chain`
arm — and re-applies it after the list-infix extension loop, so
`isa-ok * quack 5, Code` curries the user-defined infix into a WhateverCode
and `*.split("-").("a-b-c").List` wraps only the callable chain, keeping the
invocation live. The wrap logic is now shared (`wrap_finished_expr`) instead
of hand-copied per entry point.

Pin: `t/subst-accessor-element-lhs.t`. Suite status: `65_allow.t` joins the
green set (1022/1022, 28/33 files fully passing); remaining failures are
85_util, 90_csv (csv() header semantics), 91_csv_cb, 92_csv_encoding, and
99_meta (needs external Test::META).
