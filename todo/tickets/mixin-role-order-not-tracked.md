# Mixin role application order is not tracked, so multi-role precedence is wrong

Found while adjudicating ADR-0019 Phase E box E1a's V2 verification item
(`todo/deep/adr0019-e1-typeid-receiver-owner.md`) against raku.

## What raku does

```
$ raku -e '
role A { method m { "A" } }
role B { method m { "B" } }
my $x = (0 but A) but B;
say $x.m;
'
B
```

The most-recently-applied role wins a method-name collision: `(0 but A) but B` answers
`.m` from `B`, and `(0 but B) but A` answers from `A`. This matches Rakudo's documented
mixin semantics (a later `but`/`does` layers over an earlier one).

## What mutsu does

`Value::Mixin(inner, mixins)` stores composed roles as `HashMap<String, Value>` entries
keyed `__mutsu_role__{Name}` (`MixinOverrides`, a plain `HashMap<String, Value>` —
`src/value/mod.rs:299`). Nothing in that representation records *when* a role was
applied relative to the others in the same mixin layer.

`dispatch_mixin_method_call` (`src/runtime/methods_mixin_dispatch.rs:123-130`) resolves
a method-name collision across roles by walking `role_names.sort()` — alphabetical
order — not application order:

```rust
let mut role_names: Vec<String> = mixins
    .iter()
    .filter_map(|(key, value)| {
        key.strip_prefix("__mutsu_role__")
            .and_then(|name| value.truthy().then_some(name.to_string()))
    })
    .collect();
role_names.sort();
```

So mutsu's answer for `(0 but A) but B).m` depends on the alphabetical relationship
between the role names, not on which was applied last. For `A`/`B` this happens to
alphabetically sort the same as later-wins in one direction and opposite in the other —
it is not correct semantics, just an accident of naming.

## Impact

- Any program that composes two (or more) roles defining the same method name onto one
  value via chained `but`/`does`, relying on raku's later-wins precedence, gets the
  wrong method depending on role name spelling.
- ADR-0019 Phase E's E1a classifier (`src/runtime/receiver_class.rs`,
  `Interpreter::mixin_chain`) mirrors this same alphabetical order for its role-TypeId
  chain, specifically so it reproduces `dispatch_mixin_method_call`'s existing (wrong)
  decision rather than diverging in a way that would spuriously look like a NEW E1a
  regression. Fixing the ordering is out of scope for that shadow-only box.

## Fix sketch

`MixinOverrides` needs an ordering field — e.g. change the map to also carry (or be
replaced by) a `Vec<(String, Value)>` recording application order, or add a monotonic
sequence number per `__mutsu_role__` entry that `dispatch_mixin_method_call` (and the
future E1b/E4 resolver, which will consult the same chain) sorts by instead of by name.
Every `Value::mixin(...)` construction site (`but`, `does`, role composition helpers)
would need to thread the new sequence number through. This is bigger than a “change one
comparator” fix because the representation itself has no order to recover — it has to
be added at construction time, retroactively, everywhere a mixin layer is built.

## Repro (confirmed 2026-08-10, both directions)

`dispatch_mixin_method_call` iterates `role_names` ascending and returns on the first
matching definition, so it actually picks the **alphabetically-first** role, not
alphabetically-last. Applying the alphabetically-earlier role FIRST — so raku's
later-wins (picks `Z`, applied last) and mutsu's alphabetical-first (picks `A`) land on
different answers:

```
$ raku -e 'role A { method m { "A" } }; role Z { method m { "Z" } }; my $x = (0 but A) but Z; say $x.m;'
Z
$ target/debug/mutsu -e 'role A { method m { "A" } }; role Z { method m { "Z" } }; my $x = (0 but A) but Z; say $x.m;'
A
```

raku says `Z` (applied last, wins); mutsu says `A` (alphabetically first, wins) — a
genuine, confirmed disagreement, not just a hypothetical one.
