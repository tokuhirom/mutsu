# The call dispatcher now answers a repeat call without hashing

`exec_call_func_op`, the VM's function-call dispatcher, used to re-derive the
callee from its name on every single call: one hash probe into the name-keyed
`pos_light_call_cache`, then a second into `compiled_fns` to turn the cached key
back into a body. On `benchmarks/bench-fib.raku` those two probes were about
60% of the dispatcher's self time — the dispatcher itself being the
second-largest symbol in the profile, behind only the callee-entry path.

Both probes answer a question that barely changes. `fib`'s recursive call site
resolves to the same `CompiledFunction` on all 242,785 executions and hashed a
symbol twice to find out each time. ADR-0066 called for a cache the dispatcher
can read without hashing; this is its implementation.

## One masked load, four comparisons

The cache is a fixed 128-way array of 32-byte entries embedded directly in the
`Interpreter`, indexed by `name_sym.id() & 127`. A lookup is one masked load and
four integer comparisons against tokens that all live in the entry it just
loaded. Colliding names evict each other, which costs a miss and nothing else.

That shape was not the first attempt, and the first attempt is the interesting
part. ADR-0066 proposed a *per-callsite* inline cache, and one was built: the
call site addressed by its opcode index, a lazily-built side table on
`CompiledCode` mapping that index to a slot, and the slots in a `Vec` on the
interpreter. It worked exactly as designed — the name-keyed cache was consulted
twice in 242,785 calls of `fib` — and it removed **no retired instructions at
all**, +0.07%.

The reason is what the ADR had not accounted for: what the two probes cost is
their *dependent loads*, not their instruction count. A SwissTable probe is only
about fifteen instructions. Reaching a per-callsite slot through
`OnceLock` → side table → index array → slot vector → slot is five dependent
loads, which is what two probes cost. Trading a hash for an equally long pointer
chase buys nothing. Flattening the structure to a single load is what turned the
change from a wash into a win.

## Reusing an address into a table you do not own

An entry holds the *address* of the resolved `CompiledFunction`, which is only
meaningful while the table in hand is the same table the address came from and
that table has not been mutated since (a rehash moves every value). `CompiledFns`
is now a newtype over the map carrying an `id` drawn from a process-global
counter and re-drawn on every mutation. Ids are never reused, so one `u64`
comparison proves both facts at once — strictly stronger than the fingerprint
re-check it replaces, which proved neither, and it costs a compare instead of a
hash probe. `DerefMut` is deliberately not implemented, so every mutation has to
go through an entry point that re-draws the id.

Three further tokens ride along: an epoch bumped whenever the name-keyed cache
this memoises changes at all, and the callee name and callsite package.

## The wrong answer this turned up

Writing the ADR's own adversarial case — "a call site reached from two different
packages" — found a live bug that had nothing to do with the new machinery:

```raku
module PkgA { our sub which() { 'A' }; our sub probe() { which() } }
module PkgB { our sub which() { 'B' }; our sub probe() { which() } }
say PkgB::probe();  # B
say PkgA::probe();  # B  -- rakudo says A
```

`which` is a different routine in each package and the full resolver knows it,
because it reads `current_package`. Three caches in front of the resolver did
not: `fn_resolve_cache`, `light_call_cache` and `pos_light_call_cache` were all
keyed by name alone, so whichever package called a given bare name first
answered for every other package for the rest of the run — in both directions,
and for the whole program. All three are now keyed by
`(name, callsite package)`, which is the key their contents already assumed:
`PosLightTarget::Otf` had been carrying a `callsite_package` field and checking
it by hand, and that field is now redundant with the key and gone.

`t/call-inline-cache.t` pins this together with the rest of ADR-0066's
validation list: two packages sharing a bare name, `wrap`/`unwrap` around an
already-hot call site, the same name called from a separate `EVAL` compilation
unit, and a block-local routine shadowing an outer one.

## Measured, and how

Retired instructions: −2.8% on `fib` and `bench-fib`, −1.5% on `bench-tak`,
unmoved on `method-call` and `bench-class` (which dispatch through the method
path this cache does not serve yet). `exec_call_func_op` falls from 15.9% to
7.1% of a `bench-fib` profile.

The cycle figures come from a **same-binary** A/B, and that detail is the
methodological point worth keeping. Comparing cycles across two builds could not
support any conclusion at this effect size: the same source built twice differs
by more than the change is worth — the shipped binary's own cache-disabled run
of `fib` measured 151.3 Mcycles against the baseline binary's 141.9, 6.6% apart
with identical semantics, and a cross-build A/B of this change reported anywhere
from −1.5% to +3.1% depending on which pair of builds was compared, with
`codegen-units=1` no help. Building one binary with a temporary
`MUTSU_CALL_IC=0` switch and alternating it against itself holds codegen,
inlining and layout exactly fixed: −2.9% cycles on `fib`, −2.5% on `bench-fib`,
−1.4% on `bench-tak`, tracking the instruction counts. The switch was removed
before merge.
