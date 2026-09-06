# A `$.attr` read-modify-write is a silent no-op, not a refusal (and not a mutation)

`$.attr` inside a method is `self.attr` **itemized**. For a non-`rw` scalar
accessor the accessor hands back a bare value, so the itemization is a fresh
throwaway `Scalar` — and a read-modify-write assigns into that throwaway. The
expression still evaluates to the computed value, the attribute is unchanged,
and nothing is thrown. Only the simple `$.x = v` is compiled *without* the
itemize wrapper (it would defeat the assignment), so it alone hits the raw
accessor return and dies `X::Assignment::RO` — which
`news/2026-09/dot-twigil-accessor-assignment-consults-rw.md` already fixed.

mutsu got the RMW half wrong in **both directions at once**, in different
statement positions, which is why it read as self-contradictory for three
diagnosis passes. Measured on `main` against raku v2026.07, for `has $.x = 5`:

| form | raku | mutsu before |
|---|---|---|
| `my $r = ($.x *= 2)` | `r=10`, attr `5` | throws |
| `my $r = $.x *= 2` | `r=10`, attr `5` | throws |
| `($.x *= 2);` | attr `5` | throws |
| `$.x *= 2;` | attr `5` | throws `method 'x' is not rw` |
| `$.s ~= "z";` | attr `a` | throws `method 's' is not rw` |
| `$.x min= 1;` / `$.x //= 9;` / `$.x orelse= 9;` | attr `5` | throws `method 'x' is not rw` |
| `my $r = $.x++` | `r=5`, attr `5` | `r=5`, **attr `6`** |
| `my $r = ++$.x` | `r=6`, attr `5` | `r=6`, **attr `6`** |
| `$.x++; $.x++;` | attr `5` | **attr `7`** |

The increment rows are the ones that mattered: they were the last **silent data
loss** in this family, and the ticket's own table never probed an increment —
it asserted that silent over-mutation was already gone.

## Three entry points, all measured before being touched

- **Expression-position `OP=`** keeps `CompoundAssign { target: Var(".x"),
  expanded: AssignExpr { name: ".x", … } }`. `OpCode::AssignExpr` gained a flag
  for it: the compiler sets `Compiler::dot_twigil_rmw_assign` in the
  `CompoundAssign` arm and `compile_expr_assign` *takes* it (so a nested
  assignment in the right-hand side compiles as an ordinary one).
- **Statement-position `OP=`** lowers to `__mutsu_assign_method_lvalue`.
  Contrary to the ticket, no new argument was needed: the parser deliberately
  keeps the collapsed `__ANON_STATE__` spelling when a `.` follows a bare `$`,
  because the VM resolves that exact name to `self` as the `$.foo` invocant
  carrier — so `target_var` already arrives as `Some("__ANON_STATE__")` for
  `$.x` and `Some("self")` for `self.x`. The rejection site in
  `methods_mut_method_lvalue.rs` reads it directly.
- **`$.x++` / `++$.x` / `$.x--` / `--$.x`** never reached either path: they
  compile to a raw name op with no local slot and are served by
  `try_slotless_attr_incdec`, which wrote the attribute cell unconditionally.
  It now skips both stores (cell and env) while still pushing the pre/post
  value, so the return values — which were already right — are untouched.

The decision itself lives in one place:
`Interpreter::check_dot_twigil_accessor_writable` took an `rmw` flag and now
answers `Ok(true)` = "skip your store" instead of `Err` for it. Only the runtime
can decide, because rw-ness is a property of the invocant's class.

## Scope

Deliberately confined to the `$` sigil, as the simple-assign fix was. Measured:
a non-`rw` `has @.a` accepts `@.a[0] += 10` (the accessor hands back the Array,
so an element store is an ordinary `STORE`), and `is rw` mutates in every form.
`$!z *= 2` is a private write and never enters this path.

Pinned by `t/dot-twigil-accessor-readonly-rmw.t` (17 rows, verified green under
`raku` as well as mutsu).

## What is left

`$.attr .= meth` still refuses. `.=` is an RMW too, but its origin is dropped by
the parser, and no AST shape can substitute for it: `$.s .= uc` and
`$.s = $.s.uc` lower to the *same* `AssignExpr`, and raku answers them
differently (`attr=a` vs. a throw). Choosing a carrier is a design call, recorded
with its three routes in
`todo/deep/dot-twigil-dot-assign-metaop-loses-its-rmw-origin.md`.
