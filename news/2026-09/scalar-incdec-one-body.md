# `++$CALLER::x` works: the four scalar `++`/`--` opcodes share one body

Only one of the four increment/decrement forms reached a caller's variable
(#9450):

| `sub t { OP }; my $x is dynamic = 10; t(); say $x` | Rakudo | mutsu before |
|---|---|---|
| `$CALLER::x++` | 11 | 11 |
| `++$CALLER::x` | 11 | **10** |
| `$CALLER::x--` | 9 | **10** |
| `--$CALLER::x` | 9 | **10** |

`PreIncrement`, `PostIncrement`, `PreDecrement` and `PostDecrement` each had
their own body, split across `vm_misc_coerce.rs` and
`vm_var_assign_post_incdec.rs`. The four had drifted apart:

- Only `$x++` handled `$CALLER::x` and resolved pending alias binds.
- Only `$x++` FETCHed and STOREd through a `Proxy`.
- `$x--` reported a readonly operand as `postfix:<++>`.
- The prefix forms and the postfix forms walked different sigilless-alias
  helpers.
- The prefix forms stored the result with a hand-written sequence rather than
  the shared read-modify-write tail.

The four operators differ in exactly two things: which way the value steps,
and whether the old or the new value is the result. The new
`src/vm/vm_scalar_incdec.rs` therefore has one `exec_scalar_incdec_op`,
parameterized by an `IncDec { increment, prefix }`. The dispatch arms call it,
and the four old bodies (about 330 lines) are gone. The indexed forms already
shared one body. Their prefix arms now also mirror an attribute element into
its cell, as the postfix arms did, so `++@!a[0]` and `@!a[0]++` store the same
way.

Regression test: `t/vm/writeback/scalar-incdec-forms-agree.t` covers
`$CALLER::x` for all four forms, a Proxy for all four, and the readonly
message naming the operator actually used.
