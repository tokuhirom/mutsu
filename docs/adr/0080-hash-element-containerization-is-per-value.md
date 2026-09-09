# ADR-0080: Hash element containerization is a per-value property

- **Status**: Accepted (implemented 2026-09-09)
- **Date**: 2026-09-09
- **Deciders**: tokuhirom, Claude
- **Related**: [#7567](https://github.com/tokuhirom/mutsu/issues/7567) (the originating
  divergence), [ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md)
  (store-side aggregate itemization), [ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md)
  (element-container aliases), [ADR-0057](0057-var-reflection-identity-cell-address.md)
  (`.VAR` identity)

> A real Raku `Hash` stores each value in a `Scalar` container, but a slurpy
> `*%h` binds its named arguments raw. The distinction is per value: assigning
> into a slurpy hash creates a container for the new value without
> containerizing the values that arrived from the call. A hash-wide flag cannot
> represent that mixed state. mutsu keeps its existing optimized representation
> for primitive values whose container status is not observable by the affected
> path; this ADR adds the explicit per-value marker needed for Boolean `.raku`
> output.

## 1. Context

### 1.1 The divergence

The smallest reproducer is:

```raku
sub foo(*%h) { say %h.raku }
foo :a1:b2
```

Rakudo prints `{:a1, :b2}`; mutsu prints
`{:a1(Bool::True), :b2(Bool::True)}`. The same distinction is visible through
an explicit Pair:

```raku
sub foo(*%h) { say %h.raku }
foo a => True, b => False
```

Rakudo prints `{:a, :!b}`. The ordinary hash-literal case must remain unchanged:

```raku
my %h = a => True, b => False, c => 1
say %h.raku
```

Both implementations print `{:a(Bool::True), :b(Bool::False), :c(1)}` today.
Therefore changing `Hash.raku` to abbreviate every `Bool` would fix the slurpy
case by breaking the ordinary Hash case.

The discriminator is the value's container status, not the hash's origin. These
probes were re-run against mutsu `2690509c3` and Rakudo v2026.06 on 2026-09-09:

| Probe | mutsu | raku |
| --- | --- | --- |
| `foo :a, :b(1)` then `%h.raku` | `{:a(Bool::True), :b(1)}` | `{:a, :b(1)}` |
| `%h<a>.VAR.^name` in the slurpy | `Bool` | `Bool` |
| `%h<c> = True; %h<c>.VAR.^name` | `Bool` | `Scalar` |
| `%h<c> = True; %h.raku` | `{:a(Bool::True), :c(Bool::True)}` | `{:a, :c(Bool::True)}` |
| `my %g = %h` | long-form values | long-form values |
| `my %j := %h` | raw values | raw values |

The `.VAR` result for the pure slurpy case is not evidence that the model is
implemented: mutsu answers `Bool` for the mixed assigned value too. The
assignment row is the control that proves the missing state is per entry.

### 1.2 Why the current representation is insufficient

`HashData` currently stores `HashMap<String, Value>` and a single
`bare_values` bit. `Value::hash_bare_values` uses that bit for a slurpy
`*%h`, a `Map`, and other associative values whose entries are not containers.
`Value::hash` uses the opposite setting for an ordinary mutable Hash and
itemizes aggregate values at the store.

That bit is useful for the initial construction decision, but it cannot express
this valid state:

```text
{:a(raw Bool::True), :c(Scalar(Bool::True))}
```

The tempting alternative, a hash-wide `"came from *%h"` marker consulted by
`Hash.raku`, fails for exactly this example. It also cannot survive assignment
copy versus binding correctly: `my %g = %h` must create fresh value containers,
whereas `my %j := %h` must retain the raw per-entry state.

The existing value representation already has the required vocabulary. An
itemized Array or Hash carries a per-value kind/flag, and a scalar item is
represented by `Value::Scalar`. The missing case is a primitive value such as
`Bool::True`, for which the current store optimization keeps the value bare
because most consumers cannot observe the wrapper.

## 2. Decision

Represent Hash element containerization on each stored value word.

1. A normal mutable Hash stores Boolean values in a Scalar container so their
   status is available to `.raku`. Existing aggregate itemization remains
   represented by the existing Array kind, Hash flag, Seq view, or other
   established itemization representation. Other primitive values retain
   mutsu's optimized direct representation and are promoted through the
   existing element read/write paths when a container alias is required.
2. `Value::hash_bare_values` keeps values raw when constructing a slurpy hash,
   a Map, a Match capture map, or another explicitly bare-valued associative
   object. It is an input/construction policy, not a claim that all later writes
   to the hash remain raw.
3. The hash-element write funnel containerizes a new or replaced non-cell value.
   If the entry is already a `ContainerRef` created by `:=`/`:p`, assignment
   writes the raw value through that cell instead of nesting another Scalar.
4. A normal assignment copy creates a fresh HashData and therefore fresh
   per-entry container words. A bind keeps the same HashData and its per-entry
   words. `.Map`, capture-map construction, and other value-copy operations
   decontainerize entries at their existing copy boundary and then explicitly
   choose the bare-valued constructor.
5. Hash reads, `.pairs`, `.kv`, and iteration preserve the value word's status
   until an operation explicitly asks for a value copy. Existing
   `deitemize_element`/decontainerization chokepoints remain the one place that
   strips it for Map-like value semantics. No reader infers containerization
   from a variable sigil or from a hash-wide origin bit.
6. `Hash.raku` and `Hash.gist` render the stored value's status. A raw Boolean
   may use the adverbial `:a`/`:!a` form; a Scalar-wrapped Boolean uses the
   existing long form `:a(Bool::True)`/`:a(Bool::False)`. This keeps the
   ordinary Hash-literal output and the mixed slurpy output correct without a
   special case for `*%h`.

This extends ADR-0040's store-side rule; it does not supersede it. ADR-0040
decided *when* an aggregate is itemized. This ADR supplies the missing
per-entry container state for Boolean values and for the raw/containerized
boundary of a slurpy Hash.

## 3. Options considered

| Option | Mixed state | Copy/bind distinction | Consumer coverage | Verdict |
| --- | --- | --- | --- | --- |
| Abbreviate every Boolean in `Hash.raku` | No | No | Renderer only | Rejected; regresses ordinary Hash literals |
| Keep a hash-wide slurpy/origin flag | No | No | Requires compensators at every reader | Rejected; cannot represent `:a` raw plus `:c(Bool::True)` containerized |
| Add a parallel raw-key side table to `HashData` | Yes | With careful key propagation | Every value hand-out must carry the side-table bit | Rejected-leaning; duplicates value representation state and is fragile across rebuilt maps |
| **Use the existing per-value itemization representation** | **Yes** | **Yes** | **Yes, through ordinary Value flows** | **Chosen** |

The chosen representation keeps the distinction attached to the value that is
actually handed to a reader. It avoids making `ValueView::Hash` or the
`HashMap` key type a second representation system, and it composes with the
existing Array/Hash itemization and `ContainerRef` alias mechanisms.

## 4. Implementation slices

### Slice 0 — acceptance oracle

Add a focused dual-oracled test under `t/` covering:

- adverbial named arguments and explicit `Pair` arguments;
- `True`, `False`, integers, strings, aggregates, and `Nil` values;
- `%h.raku`, `%h.gist`, `%h<a>.VAR.^name`, `.pairs`, `.kv`, `.values`, and `.Map`;
- assignment copy versus `:=` bind;
- assignment into an initially raw slurpy entry and insertion of a new key;
- ordinary Hash literals as a negative control;
- promoted `ContainerRef` entries and typed/object hashes as compatibility
  controls.

The mixed row must be asserted separately from the pure slurpy rows so a
hash-wide shortcut cannot pass the test.

### Slice 1 — value-level construction policy

Introduce one Hash-specific helper for converting a value at a real Hash store
boundary. Extend `Value::hash` to apply it to Boolean values while preserving
the existing aggregate itemization and `hash_bare_values` escape hatch. Keep
the helper idempotent and make its treatment of other primitive values, `Nil`,
`ContainerRef`, and already-itemized values explicit.

### Slice 2 — mutation and alias funnels

Route every Hash element insertion/replacement, including autovivification,
`push`/`append`, splice, nested assignment, native construction funnels, and
typed/object-hash re-keying through the helper. Keep writes through promoted
`ContainerRef` cells raw. Audit direct `HashData.map.insert` calls rather than
assuming the existing aggregate-only itemization hook covers primitive values.

### Slice 3 — consumer and copy audit

Audit `Hash` indexing, `.pairs`, `.kv`, `.values`, `.Map`, flattening, method
dispatch, `.VAR`, `.raku`, `.gist`, JSON/native conversion, and hash rebuilds.
Delete or narrow any read-side compensation that becomes redundant. In
particular, prove that Map-like values decontainerize while a bound Hash keeps
the original per-entry words.

### Slice 4 — regression and full compatibility gate

Run the Slice 0 oracle against both implementations, the ordinary `t/` suite,
and the roast whitelist. Add no renderer-only Boolean exception and no
per-method fallback. Update this ADR with the accepted design and landed slices
once the implementation is verified.

## 5. Acceptance invariants

The implementation is complete only when all of these hold:

1. Raw values from `*%h` render with Pair's adverbial Boolean shorthand.
2. A value assigned into that same hash renders in the long form and reports
   `Scalar` through `.VAR`.
3. A normal Hash literal still renders Boolean values in the long form.
4. Assignment copy containerizes entries; binding preserves each entry's
   current raw/containerized state.
5. `.Map` and other explicitly bare-valued projections remove the container
   state without changing the underlying value.
6. A promoted `ContainerRef` is never wrapped in a nested Scalar, and writes
   through it remain visible to every alias.
7. Hash equality, key identity, type constraints, defaults, and cycle handling
   remain unchanged.

The implementation landed all five slices in this ADR: the dual-oracled
regression test, per-value construction policy, mutation and alias funnels,
consumer/copy audit, and the full compatibility gate.

## 6. Consequences

- Boolean values in a real Hash may incur a small per-entry allocation because
  the existing `Value::Scalar` box carries the needed status. Measure this on
  the Hash benchmark before considering a new compact representation; do not
  introduce a second side table speculatively.
- The read path becomes uniform: consumers receive a value that carries its own
  itemization status instead of re-deriving it from the source variable.
- `HashData::bare_values` remains useful for construction kinds whose *initial*
  values are raw, but it can no longer be treated as the complete semantic
  answer for an already-mutated Hash.
- The change must be audited with native and typed Hash construction because
  those paths bypass the ordinary source-level assignment syntax.

## 7. Related findings

- [#7542](https://github.com/tokuhirom/mutsu/issues/7542) and
  [ADR-0079](0079-container-itemization-is-a-holder-property-tagged-on-the-containerref-word.md)
  address itemization of a shared `ContainerRef` holder. This ADR addresses
  values inside a Hash and must not put a Hash-entry flag on the shared cell.
- ADR-0040's store-side aggregate itemization is complete. This ADR is the
  remaining primitive/per-value container-status gap exposed by slurpy named
  argument binding; it does not reopen ADR-0040's campaign.
- ADR-0036's element-container aliasing remains separate. A `ContainerRef`
  promoted for `:=` or `:p` is an aliasing representation, not a replacement
  for the per-value Scalar status decided here.
