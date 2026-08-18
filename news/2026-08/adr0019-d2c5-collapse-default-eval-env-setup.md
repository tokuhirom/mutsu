# Collapse the three attribute-default env-setup shapes (ADR-0019 D2c-5)

`has $.x = EXPR` attribute defaults were evaluated by three independently
written env-setup shapes: the pre-BUILD/post-BUILD attribute fill shared by
`dispatch_new` (`Interpreter::eval_attr_default_expr` in
`src/runtime/attr_build_defaults.rs`), the native default-constructor fast
path (`build_native_default_instance` in
`src/runtime/methods_object_default_ctor.rs`), and `dispatch_bless`
(`src/runtime/methods_dispatch_new.rs`). ADR-0019 D2c-5 scoped collapsing them
into one shared helper, gated on raku-verifying a `has_class_scoped_subs`
special case first — a class-scoped `sub` referenced from an attribute
default — to confirm all three sites actually needed identical behavior
there.

Verification with `raku -e` against each of mutsu's three construction paths
found the three sites did **not** already agree, and not only for the
class-scoped-sub case:

- `dispatch_bless` had no env setup at all — no `self`, no `?CLASS`, no
  attribute bindings, and no package switch — so a default expression calling
  a class-scoped `sub`, or defaulting to a bare nested-class type name (e.g.
  `has Inner $.x`), threw `Unknown function` / `X::Undeclared::Symbols` via
  `.bless` while the identical class worked fine via `.new`.
- `build_native_default_instance`'s package switch was gated on
  `has_class_scoped_subs`, which only checks the `class_subs` registry. A
  class-scoped `my constant` referenced from a default resolved to a bare
  package name (`BASE` printed literally) instead of its value via `.new`,
  because the guard only switches package for scoped *subs*, not for other
  package-scoped bareword lookups that also need `current_package` set.

Both were genuine correctness bugs, not just internal-shape divergence, so
the ADR's precondition resolved in favor of collapsing: `eval_attr_default_expr`
is now the single implementation, extended to also bind `__ANON_STATE__` and
the `constructing_class` marker (previously `build_native_default_instance`-only)
and to always switch the current package (dropping the
`has_class_scoped_subs`-gated skip, which was the actual source of the
constant-resolution gap). `build_native_default_instance` and `dispatch_bless`
were rewritten to call it instead of maintaining their own copies of the
setup/restore dance.

Net effect: `Foo.bless` now resolves class-scoped subs and nested-class-type
defaults exactly like `Foo.new`, and a class-scoped `constant` referenced from
a default resolves correctly through the native constructor fast path too.
`src/runtime/methods_object_default_ctor.rs` dropped from 532 to 472 lines (back
under the repo's 500-line guideline) as a side effect of removing the
duplicated inline setup.
