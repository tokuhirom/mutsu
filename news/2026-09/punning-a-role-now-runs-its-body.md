# Punning a role now runs its body, and the body's declarations survive

`Hash::Ordered` 0.0.9 goes from `red` (4/18 assertions, dying on
`Undeclared name: KV`) to **green: 1/1 baseline file, 18/18 assertions**, matching rakudo
exactly. Four independent interpreter gaps stacked behind that one file. None is specific to
the distribution; two of them reproduce with no roles at all in the failing position, and the
distribution itself was not touched.

## Calling a method on a role type object must pun the role — and punning runs its body

This was the one hiding the other three.

In Raku, a role is not a class until something puns it. Calling a method on the role *type
object* (`R.method`), or tying a container to it (`my %h is R`), puns it — and punning is a
composition, so the role's body runs at that point. mutsu's slow path
(`call_method_with_values`) already knew this. The VM's compiled dispatch path did not: it
resolved the role's method and dispatched it straight off the un-punned role, so the body
never ran at all.

The visible damage was that every method of a punned role lost its own body scope:

```raku
role R { my $x = 7; method gx() { $x } }
say R.gx;          # rakudo: 7      mutsu: Nil
```

A silently wrong answer, not an error. With a body-scoped *type* it became an error instead:

```raku
role R { my class KV { method greet() { 'KV' } }; method kv() { KV.new.greet } }
say R.kv;          # rakudo: KV     mutsu: Undeclared name: KV
```

That second shape is exactly what `Hash::Agnostic` does — its `method kv` returns
`Seq.new(KV.new(...))` over a `my class KV does Iterator` declared in the role body — and it
is why `Hash::Ordered`'s suite died four assertions in.

A `class C does R { }` consumer had always worked, which is what made this so well hidden:
class composition runs the body through `compose_role_body`, and the bindings it leaves in
the live mainline env are still there when a method later reads them. Only the pun route was
missing. The probe added to the compiled path is ordered cheapest-first — an ordinary class
receiver fails the `classes` lookup outright, and after the first pun a class *is* registered
under the name, so every later call short-circuits there too.

## The pun did not keep what the body declared

Running the body was necessary but not sufficient. `compose_role_body` persists a role body's
lexicals as the composing class's own package lexicals; the pun route had no equivalent, so
the bindings lived only in whichever frame happened to trigger the pun. For `Hash::Agnostic`
that frame is its own `method new` (the pun is reached from `bless`), which is discarded on
return — so the body ran, declared `KV`, and lost it again before `kv` was ever called. That
persistence is now one shared helper, `persist_role_body_lexicals`, used by both routes.

Recognizing a body-scoped *type* needed one more thing. `DeferredBodyOp::declared_vars`
records plain `my $x` declarations only, so `my class KV does Iterator { ... }` — a
`ClassDecl`, not a `VarDecl` — was absent from it, and the filter that drops package values
belonging to the surrounding compunit threw `KV` away. Only lexical declarations count here:
an `our class` is a package symbol, already reachable by its qualified name.

## The invocant is part of a multi candidate's identity

With the suite now reaching the end of the file, `Hash::Ordered.Str` on the *type object*
died with `Cannot resolve caller Str(Hash::Ordered:U: )` — the only candidate mutsu had left
was the `:D:` one.

`Hash::Agnostic` supplies both halves of the pair (`proto method Str(|)`, `multi method
Str(::?ROLE:U:)`, `multi method Str(::?ROLE:D:)`); `Hash::Ordered` overrides only the `:D:`
half. mutsu's role-parent shadow pruning compared candidate signatures *without* the
invocant, so the child's `:D:` override looked like a same-signature replacement for the
parent's `:U:` candidate too, and both were deleted.

The right predicate already existed — `method_signatures_match_with_invocant`, which also
requires the invocant's own type constraint to match. Both prune sites use it now. A
same-invocant override still shadows, so the duplicate candidates that pruning exists for
(the role-pun duplicate fixed on 2026-09-20 in the `Air::Plugin::Donate` work) stay pruned.

## A whatever slice on a user Associative follows *its* key order

The last failure was an ordering one, and had nothing to do with roles:

```raku
say (%h{}:v);      # rakudo: (666 314 628 271 6 7 8 42)
                   # mutsu:  (8 42 271 666 6 628 7 314)
```

The `:v`/`:k`/`:kv`/`:p` slice adverbs on a user Associative cannot use native Hash storage —
there is none — so mutsu snapshots the object's `keys` + `AT-KEY` into a plain Hash and runs
the ordinary Hash path on that. But the `*`/zen expansion then enumerated the *snapshot's*
key order, which has nothing to do with the order `keys` returned. For an ordered Associative
the values came back shuffled, disagreeing with both `%h.values` and the equivalent explicit
key slice, which were correct. The object's own key order is now carried through to the
expansion.

## Filed, not fixed

Neither is needed by the distribution, and neither is a small fix:

- [#8862](https://github.com/tokuhirom/mutsu/issues/8862) — a role body's assignment to an
  *outer* lexical is still lost on the way out of composition. The body itself runs now; the
  store to a lexical owned by an enclosing compiled frame does not reach it, which is the
  `locals`/`env` dual store. A pun has no compiled call site to reconcile at, so this needs a
  decision rather than a patch.
- [#8863](https://github.com/tokuhirom/mutsu/issues/8863) — a punned role's `:U:`/`:D:` multi
  candidates are not discriminated by the invocant at dispatch: `Y.new.g` selects the `:U:`
  body. Pre-existing, and reproducible with a role that declares both halves itself, where no
  pruning is involved at all; keeping both candidates alive simply made it observable on that
  route. Verified under `rust-gdb` that the invocant check in
  `method_args_match_for_invocant` is never reached for the punned-instance call. The same
  pair composed into a class, and a plain class declaring it, both dispatch correctly.

## Pins

- `t/oo/role/pun-composes-role-body-declarations.t`
- `t/oo/role/parent-role-multi-invocant-candidate-survives-override.t`
- `t/collections/subscript/assoc-instance-slice-adverb-key-order.t`
