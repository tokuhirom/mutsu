# A deferred `.map` callback reads its routine's parameters from the CONSUMING frame

Found 2026-09-07 while implementing [ADR-0058](../../docs/adr/0058-map-grep-produce-a-deferred-seq.md)
step 3 (extend the deferral to the listop `map` form). Step 3 was implemented,
measured green on `make test`, and then **reverted**, because it made
`roast/integration/99problems-21-to-30.t` — a whitelisted file — abort with a
stack overflow. The abort is not caused by step 3: it is a **step 2 hole that
step 3 merely routes more code onto**, and it reproduces on `main` today with
the *method* form.

## The bug

```raku
my $d = 0;
sub g(@sizes) {
    $d++; die "DEEP" if $d > 8;
    note "d=$d sizes=" ~ @sizes.raku;
    return "STOP" if @sizes == 0;
    [1].map(-> $e {
        g(@sizes[1..*]).map(-> $x { $x })
    })
}
say g((2,1)).raku;
```

```
raku:   d=1 sizes=(2, 1)   d=2 sizes=(1,)   d=3 sizes=()     then (($(("STOP",).Seq),).Seq,).Seq
mutsu:  d=1 sizes=(2, 1)   d=2 sizes=(1,)   d=3 sizes=(1,)   d=4 (1,) ... runaway
```

`g`'s recursion terminates on `@sizes == 0`. In mutsu the recursion never gets
there: at depth 3 the callback reads `@sizes` as `(2, 1)` — **depth 1's**
binding — so `@sizes[1..*]` is perpetually `(1,)`.

The parameter is bound correctly at each call (the `note` at `g`'s entry prints
the right value every time). What is wrong is the frame the *callback* reads it
from when the Seq is finally pulled: by then `g` has returned, and
`eval_map_over_items` runs the callback under whatever env is active at the
pull — which, in a recursive nest, is an OUTER invocation of the same routine.

## Why the older deferral does not have it

Replacing the callback with one that contains a `return` routes the same
program through the pre-ADR-0058 deferral (`create_lazy_map_list`, gated on
`body_contains_return` / `is_stub_routine_body`) instead, and the recursion
**terminates correctly at depth 2**. The difference is one line:

```rust
// runtime/methods_dispatch_match2.rs, create_lazy_map_list
let mut env = self.env.clone();          // <- the env at the `.map` CALL
...
let list = crate::value::LazyList { body: Vec::new(), env, .. };
```

`SeqSource::MapGrep { items, func, fatal }` carries no equivalent. So the two
deferral mechanisms that ADR-0058 step 4 wants to collapse into one disagree
about the most basic property of a deferred callback: which lexical frame it
belongs to.

## Why it is deep, and what a fix has to weigh

The obvious fix — give `SeqSource::MapGrep` an env snapshot and restore it
around `eval_map_over_items` in the pull arm (`vm/vm_helpers_lazy.rs`) — is
exactly what `create_lazy_map_list` does, and it is *already* paid for on the
`return`/stub path. But that path is rare; ADR-0058 made **every** `.map`
deferred, so an `Env` clone would move onto the hot path, and `env_deep_copies`
is a tracked `MUTSU_VM_STATS` counter for a reason. Measure it before adopting
it (the counter is optimization-independent, so iterate on the debug build —
see CLAUDE.md's build-profiles section).

Cheaper shapes worth measuring first:

- capture only the callback's own free variables rather than the whole env —
  `CompiledCode::free_var_syms` already names them, and `SubData` already
  carries a closure env that is evidently *not* getting `@sizes` (a parameter
  living in a local slot, resolved through the frame at call time);
- or make the closure capture of a routine PARAMETER work at deferral time, so
  no env snapshot is needed at all. That is the same family as ADR-0055's
  unvouched-capture cell and #7432's `unit_scope_lexical_bind`, and it is the
  architecturally cleaner answer if it is affordable.

## What this blocks

- **ADR-0058 step 3** (the listop `map` form, and then grep). The step-3a diff
  is small and was measured green on `make test` (3757 files / 39156 tests)
  before the roast run found this; see ADR-0058 §9.
- **ADR-0058 step 4**, which retires `create_lazy_map_list` — impossible while
  it is the only deferral that gets the frame right.

## Not the same thing as the nested-render hole

A second, unrelated bug found in the same session — pulling a deferred `MapGrep`
left the deferred `MapGrep`s it *produced* unpulled, so `.raku` / `.gist` /
`.Str` / `.flat` read the empty seed one level down — is fixed and pinned by
`t/nested-deferred-map-seq-is-pulled.t`
(`news/2026-09/nested-deferred-map-seq-is-pulled.md`). That fix is in this
file's way only in that its listop row is `todo`-marked until step 3 lands.

## Affected files

- `src/value/seq_body.rs` — `SeqSource::MapGrep`, which would gain the frame
- `src/vm/vm_helpers_lazy.rs` — the `MapGrep` pull arm, which would restore it
- `src/runtime/methods_dispatch_match2.rs` — `create_lazy_map_list`, the
  mechanism that already does it, and `dispatch_map_method`, which does not
- `src/runtime/builtins_collection_mapgrep.rs` — `builtin_map`, the step-3a site
- `roast/integration/99problems-21-to-30.t` — the whitelisted witness
