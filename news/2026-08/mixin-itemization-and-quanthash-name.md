# A `but`-mixed container keeps its `$` itemization, and a mixed quanthash keeps its role

A container held in a `$` renders itemized — `.raku` prefixes it with `$`.
Mixing a role in dropped the prefix:

```raku
role R { }
my $h  = { a => 1, b => 2 };
my $mx = { a => 1, b => 2 } but R;
say $h.raku;     # ${:a(1), :b(2)}
say $mx.raku;    # was: {:a(1), :b(2)}   raku: ${:a(1), :b(2)}
```

Only the rendering was ever wrong — the value behaved correctly throughout,
including through subscripts. The itemization a `$` confers is applied to the
value *as it is stored* (`itemize_scalar_store`, and `itemize_value` for the
general `$(...)` form), and both matched on the container variants directly. A
`Mixin` is its own variant wrapping the container, so it fell through the match
untouched. Both now itemize through the wrapper and re-wrap, which fixes hashes,
arrays and lists together.

## A mixed Set/Bag/Mix names the role in its own type

The reverse case, and the one the ticket asked to check alongside. raku does not
itemize a quanthash — but a quanthash *does* name its own type in `.raku` and
`.gist`, and a mixed one names the role with it:

```raku
say (set(<a>) but R).raku;    # was: Set.new("a")   raku: Set+{R}.new("a")
say (set(<a>) but R).gist;    # was: Set(a)         raku: Set+{R}(a)
```

`.^name` already answered `Set+{R}`; the two renderers hardcoded the name from
the variant instead. They now take it from the caller, which is the only place
that holds the `Mixin` and can ask `what_type_name` for the suffixed form. Three
call paths needed it — the recursive `raku_value`/`gist_value` walks, and the
direct `.raku`/`.gist` method dispatch, which unwraps the mixin and delegates to
the inner value the same way `^name` would without its own arm.

One consequence worth naming: an empty *immutable* Set/Bag/Mix renders via its
lowercase coercer (`set()`, `bag()`, `mix()`), but a mixed one cannot — there is
no coercer spelling that carries a role — so it falls back to the long form,
`Set+{R}.new()`. That matches raku.

An *anonymous* role (`but role { }`) still renders un-suffixed rather than
raku's `Set+{<anon|1>}`, consistent with what `.^name` already does: mutsu's
internal anon id would not match raku's, so leaking it would be worse than
omitting it.

Pinned by `t/mixin-itemization-raku.t` (27 assertions, all of which also pass
under `raku`).
