# `when SomeUndeclaredType { ... }` should be a parse-time `X::Comp::Group`, but the safe fix needs a cross-file type index

## Repro

```raku
given 42 { when SomeUndeclaredType { 1 }; default { 0 } }
```

```
raku : ===SORRY!=== Function 'SomeUndeclaredType' needs parens to avoid
       gobbling block (or perhaps it's a class that's not declared or
       available in this scope?)
       Missing block (apparently claimed by 'SomeUndeclaredType')
mutsu : runs to completion with no error (falls through to `default`)
```

raku's parser cannot tell, for an undeclared bareword immediately followed
by `{`, whether it names a type (smart-match target) or a routine (taking
the block as its sole argument) — so it raises a compile-time
`X::Comp::Group` bundling an `X::Syntax::BlockGobbled` sorrow and an
`X::Syntax::Missing` panic. mutsu does not diagnose this at all: it parses
`SomeUndeclaredType` as a bareword `when` condition and silently falls
through when it does not smart-match.

## Where it showed up

`todo/deep/vendor-real-test-module.md`'s `t/` residue sweep:
`t/undeclared-when-type.t` and `t/exception-role-membership.t` both assert
`throws-like 'given 42 { when SomeUndeclaredType { 1 }; default { 0 } }',
X::Comp::Group`. It is not a `Test`-shape difference — the repro above
reproduces with no `Test` module in sight.

## What already exists, and why the obvious extension is unsafe

`when_stmt` (`src/parser/stmt/control/given_when.rs`) already has exactly
this "gobbled block" diagnosis, but deliberately scoped to bareword names
under the `X::`/`CX::` reserved namespaces only:

```rust
if rest.starts_with('{')
    && let Expr::BareWord(name) = &cond
    && (name.starts_with("X::") || name.starts_with("CX::"))
    && !crate::runtime::utils::is_known_type_constraint(name)
    && !crate::runtime::utils::is_known_compound_type(name)
    && !crate::parser::stmt::simple::is_user_declared_type(name)
{
    return Err(gobbled_block_error(name, rest.len()));
}
```

The tempting fix is to drop the `X::`/`CX::` namespace restriction and run
the same three checks for *any* bareword. **Investigated 2026-08-19, and
this is unsafe as-is.** A survey of `modules/` (the vendored batteries)
finds real `when SimpleTypeName { ... }` usages where the type is declared
in a *different file* of the same distribution, loaded via `use` at
runtime — e.g. `Cro::HTTP::ResponseParser`'s `when Header { ... }` (`Header`
is not declared anywhere in that file). `is_user_declared_type`
(`src/parser/stmt/simple/pragma_preseed.rs`) only tracks types the
*current* file declares with `class`/`role`/`enum`/`grammar` during
parsing — mutsu registers imported/cross-file types at run time, not parse
time (the existing code comment already gives the identical reasoning for
why compound names like `Day::Mon`, a user-enum value, are excluded too).
Broadening the check as written would misdiagnose every one of those
cross-file cases as a genuine parse-time "gobbled block" error — a real
regression across the batteries corpus, not a synthetic risk.

Quick survey commands, for whoever picks this up:

```bash
grep -rEo "when [A-Za-z_][A-Za-z0-9_:]*\s*\{" t/*.t roast/**/*.t modules/ 2>/dev/null \
  | sed -E 's/^[^:]+://' | sort | uniq -c | sort -rn
```

## What a correct fix needs

Either:

1. A cross-file/cross-compunit type-name index available at parse time —
   e.g. a pre-pass that scans every `use`d module's exported type
   declarations before the main parse, or a lazily-populated global type
   registry shared across compunits in the same process. This is a real
   architectural addition (module loading currently happens at runtime,
   not parse time — see the "Raku's context-dependent parsing (slangs)"
   and module-loading sections of `CLAUDE.md`), not a small change.
2. Some other way to distinguish "declared nowhere reachable" from
   "declared in a sibling file not yet parsed" without a full index — not
   identified this session.

Do not attempt option 1 as a side effect of another change; it touches the
same module-loading machinery several other deep tickets already depend on
and deserves its own design pass.
