# A method's env merge stops re-writing 26 entries it never changed

Third slice off `todo/perf/adr0019-g3-diffuse-bless-allocation-cost.md`, and the second one
found by pointing `alloc_scope!` at a region rather than guessing what was in it. The
ticket's own next-item list turned out to be wrong about one item and right about another;
this entry records both.

## The item that was misdiagnosed

The ticket named `bless:named-args` (11 allocations per `bless` on `benchmarks/bench-ctor.raku`)
and proposed a cause: an O(attrs x args) linear scan over `plan.class_attrs` that wanted an
index. Splitting that loop into sub-scopes attributed **all 11 allocations to the `%`-sigil
attribute coercions and exactly zero to anything else** — not the scan, not the
`attributes.insert`, not the `deferred_defaults.retain`. Indexing the scan would have
bought nothing.

The real cause is that `HashData::map` is a `HashMap<String, Value>`, so copying a hash
allocates one `String` per key. Confirmed by scaling: constructing an object with one `%`
attribute 1000 times costs 58,512 allocations with a 1-key hash and 67,508 with a 10-key
one — exactly one allocation per extra key per copy. That is a structural property of
mutsu's hash representation, not a `bless` problem, and the fix (an `Arc<str>` or interned
key type) touches every site naming `HashMap<String, Value>`. Filed as
`todo/deep/hash-copy-allocates-a-string-per-key.md` with the measurements and the
"measure before building it" caveat, rather than forced into an undersized fix here.

## The item that was real: `mfast:epilogue`

The compiled method-call epilogue cost 4.9 allocations and 815 bytes per call. Sub-scoping
it put 3.5 of those in the env-restore branch, and 4.0 per merging call inside
`merge_method_env` itself.

A temporary probe printing the merged key set answered why. `submethod TWEAK(:$!spec) { }`
— an empty body — merged **26 entries** back into its caller on every single call:

```
*CWD  *TMPDIR  @*ARGS  ?FILE  $*OUT  $*ERR  *IN  *PROGRAM-NAME  $*CWD  $*TMPDIR
Dist  *SCHEDULER  *HOME  %*ENV  *OUT  *REPO  $*IN  *PROGRAM  *ARGFILES  Spec
$*HOME  *ERR  Any  d  $*ARGFILES  =pod
```

None of them is a write. They are there for the reason a comment a few lines below already
gave: a nested method call flattens the scoped overlay, copying every parent
lexical/global into the callee's overlay, so the merge sees them as callee-overlay entries
and — because the caller has the same key — keeps them. Each one was then re-inserted into
the caller env, paying for the `writes` Vec and for the caller overlay's `cow_mut` clone.

The `changed_caller_locals` scan right below already computed the answer for a subset of
these: `cheaply_unchanged(old, v)`, an O(1) conservative identity test that only returns
true when it can *prove* the values are the same. If it proves them the same, re-inserting
is a no-op. So the collect now drops such an entry outright:

```rust
if saved.get_sym(*k).is_some_and(|old| cheaply_unchanged(old, v)) {
    return None;
}
```

This cannot change `changed_caller_locals`, which pushes a key only when `saved.get_sym` is
absent or `cheaply_unchanged` is false — exactly the entries the new test keeps. Nor can it
change what the caller observes: a container the callee mutated in place keeps its `Gc`
identity, so the caller already points at the mutated container and the merge was a no-op
for it before too.

## Measurements

Allocations (`--features alloc-stats`, exact and load-independent), `bench-ctor`:

| | before | after |
| --- | --- | --- |
| `mfast:epilogue` per method call | 4.9 allocations / 815 bytes | 2.2 / 205 |
| whole program | 1,444,372 allocations / 111.4 MB | 1,404,374 / 102.2 MB (-2.8% / -8.2%) |

Order-swapped wall-clock A/B, `MUTSU_JIT=off`, min-of-13 then min-of-11, both binaries
built from the same tree:

| benchmark | round 1 (new first / base first) | round 2 (new first / base first) |
| --- | --- | --- |
| `bench-ctor` | -3.6% / -2.8% | -3.5% / -5.9% |
| `bench-class` | -2.0% / -4.1% | — |
| `method-call` | -8.3% / -8.1% | -2.4% / +6.3% |
| `bench-fib` | — | -9.0% / -14.5% |
| `poly-call` | — | -0.3% / -0.4% |
| `bench-yaml-parse` | — | +0.6% / -9.8% |

`bench-ctor` and `bench-fib` are consistent in every order; `method-call`'s second round
flips sign, so treat it as no worse than flat. Nothing regresses consistently.

`t/method-env-merge-unchanged-globals.t` pins the semantics the skip must not break: a
method that genuinely writes a caller lexical, a dynamic, and a global still propagates it,
and one that only reads them does not disturb the caller.
