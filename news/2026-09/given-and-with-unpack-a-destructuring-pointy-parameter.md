# `given` and `with` unpack a destructuring pointy parameter

Working the ecosystem parity cluster [#7988](https://github.com/tokuhirom/mutsu/issues/7988),
continuing from the leads the previous run left as "still untouched". The named lead was
`::?CLASS` in a parameter (ASTQuery); minimising it found two separate gaps, one of them a
silently wrong answer rather than a parse failure.

## A destructuring pointy parameter bound nothing

`ASTQuery::Match` merges two match objects like this:

```raku
given $m -> ::?CLASS:D (:@list, :%hash, |) {
    $new.list.append: @list;
    for %hash.kv -> $key, $value { $new.hash.push: $key => $value }
}
```

The module parsed and loaded. It just merged nothing: `@list` and `%hash` were never declared,
so both read empty and every merge produced an empty result. No error, no warning — the worst
shape a compatibility gap can take.

A parameter carrying a `sub_signature` unpacks one bound value into several lexicals, and that
unpack is the same operation wherever it appears. mutsu had three copies of it, in three states
of completeness:

- **`for`** had the complete one: a named sub-parameter binds through the accessor method when
  the object has one and falls back to a hash key when it does not, `@`-sigil sub-parameters
  flatten, `|` captures collect the tail, renames (`:key($k)`), defaults and optional markers all
  work.
- **`with`** had a copy that called a method named after the sub-parameter *with its sigil still
  attached* — `$obj.@list`, which is not a method anyone has — and knew nothing of hash fallback,
  captures, renames or defaults.
- **`given`** had no copy at all. `pointy_topic_bind` bound the topic to the parameter's synthetic
  name and dropped the sub-signature on the floor.

The `with` copy was not strictly weaker, though: it was the only one of the three that applied a
sub-parameter's **coercion** type, so `with (1, 2) -> (Str() $a, $b)` bound a `Str` where `for` and
`given` bound an `Int`. The shared lowering does that too now, so the other two gained it. Only a
real coercion coerces — the constraint is recorded as `Target()` or `Target(Source)`, a plain
nominal constraint is left alone (rakudo type-checks it rather than converting), and an indirect
`::(EXPR)` constraint, which also ends in `)`, is excluded.

The `for` copy is otherwise the only one: it moved to `src/param_destructure.rs`, and `given` and
`with` call it. `given` declares the topic under the parameter's synthetic name first — exactly as `for`
declares `__for_unpack` — and the binds read from that name, so the two constructs differ only in
where the value comes from.

`if` / `elsif` / `unless` are **not** fixed by this and are filed as
[#8340](https://github.com/tokuhirom/mutsu/issues/8340). Their problem is one layer earlier:
`parse_if_binding_params` strips a leading `(` as though it were the parameter list's own
parentheses, so `if EXPR -> (:key($k))` never becomes a destructure in the first place, and
`lower_if_clause_binding` then calls the clause with a `|`-slipped condition where rakudo passes
one argument. Both halves have to move together, and the second changes `if COND -> $a, $b` for
everyone who writes it.

## `::?CLASS (…)` in a routine signature

The second gap is a parse failure, in the other position the same construct can appear in:

```raku
class C { method m(::?CLASS:D (:$a, |)) { $a } }
```

rakudo accepts it; mutsu answered `Confused. expected ')'`. The parameter parser handles
`::?CLASS` / `::?ROLE` in a branch of their own, entered before the general type-constraint path
can see them. That branch knew a variable (`::?CLASS $x`), an invocant marker (`::?CLASS:D:`) and
a bare anonymous parameter (`multi prefix:<-->(::?CLASS)`) — and read everything else, `(`
included, as the bare form, leaving the `(` unconsumed and failing the enclosing signature.

A `(` after the pseudo-type is not that branch's business. It opens either a coercion or a
destructure, and the general path below already parses both in full, together with the `?`/`!`
marker, `is` traits, `where` clause and default that may follow —
`parse_type_constraint_expr` reads `::?CLASS` and its smiley itself. The branch now declines to
consume, and the parameter reaches the path that can read all of it.

## Tests

- `t/routines/signature/given-with-destructuring-pointy-param.t` — 21 assertions over both
  constructs: attribute accessors, hash-key fallback, renames, defaults, optional sub-parameters,
  `|` captures, `without`, coercion types (and a plain nominal constraint, which must *not*
  coerce), and the plain (non-destructuring) pointy parameter's topic aliasing, which must keep
  working.
- `t/routines/signature/destructure-pseudo-type-constraint.t` — 10 assertions over the
  pseudo-type parameter forms, including the invocant and bare-parameter spellings the branch
  already handled.

Both verified green against rakudo.
