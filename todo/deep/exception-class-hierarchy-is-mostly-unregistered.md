# 124 core `X::` exception classes are not registered as types

mutsu registers 77 `X::` classes in `src/runtime/runtime_init.rs` (the
`register_x(name, parent)` block). It *raises* far more than that: every one of
the 124 core exception classes below appears in mutsu's own source, but none is
a real type, so `.new` on it does not exist.

```
$ mutsu -e 'say X::Bind::Slice.new.^name'
X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on X::Bind::Slice
$ raku  -e 'say X::Bind::Slice.new.^name'
X::Bind::Slice
```

That is not only a user-facing gap. mutsu's own compiler emits
`X::Bind::Slice.new(...)` (`src/compiler/expr_closure.rs`), so the *raise* fails
with `X::Method::NotFound` and `throws-like … , X::Bind::Slice` sees the wrong
class — `t/bind-to-whatever-index.t` and `t/indexed-bind-in-expression.t` both
fail this way under rakudo's real `Test` module
(`todo/tickets/vendor-real-test-module.md`).

## How the list was measured

Every `X::…` name used as an expected type in a `throws-like` / `isa-ok` /
`fails-like` across `t/` and `roast/` (217 names), minus those whose `.new`
already works, minus those `raku` does not have either (test-local classes such
as `X::Boom`, `X::Meow`) — leaving 124. All 137 failures had the identical
signature (`Unknown method … new on <class>`), i.e. every one is *unregistered*
rather than merely fussy about its attributes.

## Why this needs design, not a mechanical sweep

The obvious rule — parent = the longest `::`-prefix that is itself registered —
is **wrong more often than right**, so applying it would bake false inheritance
into the type system. Measured against raku:

| child | namespace prefix | is it really an ancestor? |
| --- | --- | --- |
| `X::IO::Mkdir` | `X::IO` | yes |
| `X::Comp::Trait::Unknown` | `X::Comp` | yes |
| `X::Syntax::Perl5Var` | `X::Syntax` | yes |
| `X::Bind::Slice` | `X::Bind` | **no** |
| `X::Numeric::DivideByZero` | `X::Numeric` | **no** |
| `X::Parameter::RW` | `X::Parameter` | **no** |
| `X::Attribute::Required` | `X::Attribute` | **no** |
| `X::Placeholder::Block` | `X::Placeholder` | **no** |
| `X::Syntax::Malformed::Elsif` | `X::Syntax::Malformed` | **no** |
| `X::Str::Sprintf::Directives::BadType` | `X::Str::Sprintf::Directives` | **no** |

The reason is structural: in rakudo the shared behaviour is carried by **roles**
(`X::Comp`, `X::Syntax`, `X::OS`, …), not by superclasses —
`.^parents(:local)` is plain `Exception` for 117 of the 124. mutsu's
`register_x` takes a single parent name and synthesises an MRO from it, so it
can only approximate a role. Where the approximation happens to match (the
existing `X::Syntax::*` entries) it is fine; where it does not, registering
under the prefix would make `$ex ~~ X::Bind` answer True when raku says False.

So the work is:

1. Decide how `register_x` should express role-style membership — either extend
   it with a `does` list that feeds the same `mro`/`istype` answer, or keep the
   single parent and derive it from raku's real `.^mro ∪ .^roles` per class
   rather than from the name.
2. Generate the 124 entries from that decision (the raw data is cheap to
   re-derive: `raku -e 'say X::Foo.^mro.map(*.^name); say X::Foo.^roles.map(*.^name)'`).
3. Only then check what it unblocks — most `throws-like` assertions already
   match on the class *name*, so the payoff is concentrated in the sites where
   mutsu constructs the exception itself.

A worked example of why step 3 comes first: three files in the Test-vendoring
sweep were filed here because their failure text named two `X::Undeclared*`
classes. Two of them (`block-lexical-scope.t`,
`gate-b-callee-name-collision-and-deref-capture.t`) were not hierarchy problems
at all — mutsu raised the *wrong class* for an undeclared variable and for a
call to a CORE term constant
(`news/2026-08/undeclared-variable-is-not-undeclared-symbols.md`). Registering
`X::Undeclared::Symbols` under `X::Undeclared` to "fix" them would have baked in
inheritance raku does not have. Read the failure, do not pattern-match the name.

Step 3 is worth doing first as a cheap sanity check: on the full Test-vendoring
sweep only 2 of the 9 remaining regressions are caused by an unregistered class,
so this is a correctness-of-the-type-system task rather than a
sweep-clearing one, and it should be sized accordingly.
