# `but R(42)` names the mixin `R[Int]` — the argument is an attribute, not a type parameter

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/operators.rakudoc:1977`),
re-verified against raku v2026.07 the same day.

## Repro

```raku
role Answerable { has $.answer }
my $u = 'Life' but Answerable(42);
say $u.^name;      # raku: Str+{Answerable}   mutsu: Str+{Answerable[Int]}
say $u.answer;     # both: 42
```

Minimal:

```raku
role R { has $.x }
say (1 but R(2)).^name;   # raku: Int+{R}   mutsu: Int+{R[Int]}
```

## Diagnosis

The *behaviour* is right — the attribute is initialized and `.answer` reads
back `42` — so this is purely a naming defect: `R(42)` in `but` position
initializes the role's single attribute (`Language/operators.rakudoc`'s "role
with a single attribute" form), and mutsu is recording it as if it were a role
**parameterization** (`R[Int]`), stamping the argument's *type* into the
composed name.

Since `.^name` is what `X::` messages, `.raku`, `.gist` and introspection all
report, this leaks into any program that prints a mixin's type.

## Where to look

The `but`/`does` mixin composition path (`src/vm/vm_trait_mod_does_ops.rs`) and
wherever the composed name is built. A genuinely parameterized role
(`role R[::T] { }`; `1 but R[Int]`) must keep its `[Int]`, so the fix is to
distinguish the two spellings rather than to drop the suffix unconditionally.

## Neighbourhood to check when fixing

`does R(42)` as well as `but`; a role with two attributes (`R(1, 2)`); a role
that is both parameterized and attribute-carrying; `.^roles`/`.^mro` output;
and `X::Role::Initialization` (raku throws it when the role has no attribute to
initialize — check mutsu's behaviour there too).
