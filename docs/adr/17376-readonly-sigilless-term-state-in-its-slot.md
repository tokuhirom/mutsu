# ADR-17376: A readonly sigilless term's state lives in its slot, not in the env

- **Status**: Proposed
- **Issue**: [#17376](https://github.com/tokuhirom/mutsu/issues/17376) (carried by #17331, Monad
  `t/14_concurrent_types.t`)

## Context

A sigilless term (`my \k = EXPR`) is bound to what `EXPR` produces. Raku has no container for
it: bound to a value it is readonly, bound to a container (`my \k = $x`) it writes through.
mutsu records the readonly verdict as an env key, `__mutsu_sigilless_readonly::\k`, written by
`SetTermMark` twice per declaration (`false` before the store, to clear a previous
iteration's mark, and `true` after it), and every reader asks the env by name: `CheckReadOnly`,
the `++`/`--` refusal, the binders deciding whether a caller variable can be aliased, `.=`, the
rw-loop and `map`/`grep` writeback paths.

That env write is the whole cost of the declaration. In a routine called on the light path the
frame's env overlay is a shared empty singleton until the first by-name write; the marker is
that first write, so every call of a routine with a `my \k` allocates an overlay, copies it on
write and merges it back on return. Measured on a profiling build (callgrind slope):

| routine body | per call |
| --- | ---: |
| `my $key = "a"; 1` | 6.8k Ir |
| `my \key = "a"; 1` | 17.5k Ir |

Monad's type helpers (`some-type`, `none-type`, `parameterized`, `left-type`, `right-type`)
each declare `my \key = M.WHICH ~ '|' ~ T.WHICH` and run on every constructor call, ~30 times
per iteration of `t/14`'s chain loop.

## Decision

### D1. The readonly state of a `TermMark::Readonly` declaration is a compile-time fact of its slot

The compiler already decides that `my \k = EXPR` with a non-variable `EXPR` binds a value
(`TermMark::Readonly`): the term is readonly for its whole life. `BindingFlags` records it per
slot (`readonly_term`), and any other binding of the same slot (a parameter, a bound term
`my \k = $x`, an uninitialized `my \k`) clears it.

### D2. A slot nothing outside the chunk's own ops can name keeps no env marker

`CompiledCode::lean_readonly_term_slot(idx)` holds when the slot is a readonly term and is
`!needs_env_sync` and not a duplicated name: no closure captures it, no by-name or reflective
lookup reads it. `SetTermMark` carries the slot and writes nothing for such a slot. A slot that
does not qualify keeps the marker exactly as before, so captured terms, EVAL, `CALLER::`/`OUTER::`
and parameters are unchanged.

### D3. The readers ask the slot

A readonly term is readonly whether or not its marker was written, so the readers of the
declaring chunk answer from the slot flags (`readonly_term_slot`) for every such term, lean or
not; the marker is only what other chunks still read. Only ops of the declaring chunk can reach
a lean slot, so its readers are:

- `CheckReadOnly` (assignment, list assignment, `.=`) and the `++`/`--` refusal: a lean slot is
  answered as the env marker would have been (a user `STORE` or a mutable aggregate assigns
  through; anything else is "Cannot modify an immutable ...").
- `WrapVarRef`, the op that hands a variable's container to a callee or a bind (`r(k)`,
  `my \j = k`, `:k(k)`): a readonly term has no container, so a lean slot passes its value.
  A sigilless parameter then binds it readonly, an `is rw` parameter refuses it
  (`X::Parameter::RW`, as rakudo does), and a bound term built from it is readonly — the three
  verdicts the callee used to read back from the caller's env marker.

A lean slot also takes the env-free declaration lane (#12151): its `SetVarDynamic` and
`SetLocalDecl` write the slot and nothing else.

`lean_readonly_term_slot` deliberately does not consult the program-wide
`NON_EVAL_REFLECTIVE_SEEN` latch that the `$`-scalar lane does: one symbolic lookup anywhere
(`::('Monad::Either')`) sets it, and the lookups that can name this slot are this chunk's own or
a nested closure's, which `needs_reflective_capture` already covers. A `CALLER::` reader reaches
the slot's value through the calling-frame flush as for any slot-only local.

## Consequences

- A `my \k = EXPR` in a routine or loop body costs what `my $k = EXPR` costs when nothing
  captures `k`.
- Two sources of truth for one fact: the slot flag for lean slots, the env marker otherwise.
  The split is decided per slot at compile time (`needs_env_sync` is the same fact the scalar
  declaration lane, #12151, already relies on), so a slot never switches between them.
- `WrapVarRef` of a readonly term passing a plain value also fixes `rr(k)` for
  `sub rr($x is rw)` and `:k(k)` as a writable pair value, both of which mutsu accepted.
- Residual: a deeper reflective write (`CALLER::CALLER::<\k> = 1` from a callee) to a lean term
  finds no marker. Rakudo refuses it; such a write to a readonly term is the only behaviour this
  can change.

## Alternatives considered

- **Keep the marker, make the write cheaper** (a dedicated op with a pre-interned key, the
  current `SetTermMark`): already done (#17376's first slices); the cost left is the overlay
  the write forces, which no cheaper write removes.
- **Mark the name in the frame's `readonly_vars` journal instead.** Cheaper than the env, but
  still a per-declaration write plus an undo-journal entry, and the journal is by name, so a
  callee's same-named term would need the same clearing protocol as the env marker.
- **Derive writability from the bound value for every sigilless term** (container => writable,
  value => readonly). Raku's own rule, and the eventual target, but the general binder still
  aliases some caller variables by name (`__mutsu_sigilless_alias::`) with a plain copy in the
  slot, so the value does not yet tell the truth for bound terms and parameters.

## Open questions

- Bound terms (`my \hit = $cache{key}`, `TermMark::Bound`) cost ~19k Ir more than
  `my $hit = ...`. Their verdict is a run-time one (`MarkSigillessBindSource`); moving it to the
  slot needs the alternative above (the slot holding the container itself) first.
