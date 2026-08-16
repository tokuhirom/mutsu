# `my package`/`my module` did not lexically scope like `my class`, leaking across EVALs

`my class`/`my role` declared inside a block or an `EVAL`'d string were already
lexically scoped: the class/role stays registered, but its bare *name* is
suppressed once the enclosing block/EVAL exits, via
`register_lexical_class`/`pop_lexical_class_scope`
(`suppressed_names`/`lexical_class_scopes`). `my package`/`my module` never
participated in that mechanism — they used a separate, incomplete
`block_declared_vars`/`mark_my_scoped_package_item` path — and `EVAL` itself
never pushed/popped a lexical-class-scope frame around its own body at all
(only bare `{ ... }` blocks did).

Two consequences, both found while working
`todo/tickets/vendor-real-test-module.md`'s `roast/S32-exceptions/misc.t` gap
list:

1. **`EVAL 'my package A { }'; A` stayed resolvable after the `EVAL` returned**
   (raku: `X::Undeclared`), and so did `my class`/`my role` declared inside an
   `EVAL` — `EVAL` never pushed its own lexical-class-scope frame, so nothing
   ever re-suppressed a name a snippet declared.
2. **A `package`'s `shadow_suppressed_type_with_package` un-suppression was
   permanent.** When a `my package A` shadows a stale, out-of-scope `my class
   A`, it deliberately un-suppresses `A` so the new package becomes the active
   type — but nothing ever re-suppressed it afterward. So the *first* `EVAL`
   that declared `my package A` — even one that itself failed later in the
   same snippet — permanently un-suppressed `A` for the rest of the program.
   Any *later*, wholly independent `EVAL` reusing the same short name (`my
   package A {}; sub foo(A $a) { }`, a common `throws-like` shape in
   `roast/S32-exceptions/misc.t`) then found `A` already "resolvable" and
   silently skipped the `X::Parameter::BadType` check it should have raised.

Fixed generally, not per-symptom:

- `EVAL` (`parse_and_eval_with_operators`, `src/runtime/system.rs`) now
  pushes/pops its own lexical-class-scope frame around the snippet's body,
  mirroring the bare-block cleanup in `vm_misc_scope.rs` exactly (pop runs
  unconditionally, so a snippet that dies partway through is still cleaned
  up).
- `RegisterPackageMy` (`my package`/`my module`, `src/vm/vm_exec_dispatch.rs`)
  now also calls `register_lexical_class`, so a `my`-scoped package
  participates in the same scope-exit re-suppression `my class` already had —
  both for a plain bare block and for `EVAL`'s own new push/pop.

Pinned by three new assertions in `t/lexical-type-scope-suppression.t`: a
second, independent `EVAL` of `my package A` still raises `X::Parameter::BadType`
after an earlier, unrelated `EVAL` of the same short name already ran (the
exact `misc.t` order-dependent shape), and `my package` declared in a bare
block does not outlive it. `roast/S32-exceptions/misc.t`'s line-227 assertion
(tracked in `todo/tickets/parameter-badtype-order-dependent-under-many-prior-evals.md`)
now passes deterministically, including with the file's full preceding ~47
subtests run for real, both under the native `Test` provider and
`MUTSU_REAL_TEST=1`.

**Not fixed, deliberately out of scope:** a *non*-`my` `class`/`package`
declared inside `EVAL` (e.g. `EVAL 'class Foo { }'`) also stays visible by
bareword outside the `EVAL` in mutsu, which raku does not do either — but that
is a separate, broader gap (`EVAL` as a whole compilation unit, not
specifically lexical-scope bookkeeping) with no test in the current suite
depending on it either way; changing it risks regressing legitimate
"declare in a block, keep visible outside the block" package semantics
(`{ package Foo { } }; Foo` staying visible IS correct raku behavior — only
`EVAL`'s own boundary is different). Left as-is.
