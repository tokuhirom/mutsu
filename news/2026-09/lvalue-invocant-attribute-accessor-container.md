# An lvalue method call's invocant can now be an rw attribute accessor

ADR-0067's E6 row is closed. `class C { has $.v is rw }; my $c = C.new(v => 42);
$c.v.snitch = 9` now writes `9` into `$c.v`, matching raku; before this it died
with `X::Assignment::RO: cannot assign through .snitch on non-instance`.

## What was actually missing

Nothing about the write. ADR-0067's slice 3a already made a raw invocant arrive
as a container and taught `assign_method_lvalue_with_values` to write through it,
and ADR-0059's `assign_lvalue_container` has always been the one consumer. What
E6 lacked was a **producer**: an invocant that is itself a method call was
compiled as an ordinary rvalue read, so the attribute's Scalar never reached the
call. mutsu already had the producer for exactly that container — the
`MarkAccessorRefContext` marker that makes `my $x := $c.v; $x = 9` write through
today — it was simply never emitted before an lvalue invocant.

So the change is one insertion in the compiler: when a
`__mutsu_assign_method_lvalue` call's argument 0 is an argument-less method
call, mark that call as wanting a container. Three producers now feed the single
consumer ADR-0059 built, which is the shape the ADR predicted for its "one rule,
four mechanical parts".

## Why an unconditional emission is safe

Rawness is not statically known — `$a.snitch`'s callee depends on `$a`'s runtime
type, and for the dynamic spellings on a runtime method-name string — so the
marker goes out for *every* `$obj.acc.m = v`. The narrowness lives in the
consumer instead: `try_fast_accessor_read` hands back a container only for a
zero-argument read of a public `is rw` **scalar** attribute accessor, and
ignores the flag entirely otherwise. `--dump-bytecode` confirms that
`benchmarks/method-call.raku`, `benchmarks/bench-class.raku` and a `$p.x = $i`
loop compile byte-identically with and without the change.

## Two things measurement changed

**Slice 3a's "single chokepoint" was not single.** 3a decontainerizes the target
at one point right after the raw-invocant branch declines, so that the ~40
`Instance`/`Array`/`Hash` branches below it keep seeing plain values. One such
branch sat *above* it — the IO::Path `.SPEC`/`.CWD` read-only guard — and with a
container invocant (`class C { has IO::Path $.p is rw }; $c.p.SPEC = 5`) its
diagnostic degraded to a bare `No matching candidates for method: SPEC`. It now
sits below the chokepoint, where the rule always said it belonged.

**The marker had to become a runtime-gated op, and it took three measurements
to get the gate right.** Emitting the plain `MarkAccessorRefContext` cost ~14%
on a tight `$o.i.w = $n` loop, because the container-producing branch runs an
MRO walk and promotes the attribute slot on every iteration to mint a container
the chokepoint then discards (`.w` is not raw). Gating on "could *any* callee be
raw" fixed 6.d but not 6.e, where the native `snitch` row exists for every
method name — and asking that question called `current_language_version()`,
which clones a `String`, so it cost ~29% in heap allocation alone (there is now
a non-allocating `current_language_version_starts_with`). The shipped
`MarkLvalueInvocantRefContext` therefore carries the *outer* method's name,
which the compiler knows in every spelling but the dynamic one, making the
runtime test character-for-character slice 3a's own filter one op earlier.

Final same-binary env-switch A/B on a release build, min of 15 interleaved
pairs: `$o.i.w = $n` is +1.9% under 6.e and −4.4% under 6.d. Both are inside the
noise floor, which the control calibrates directly — `$p.x = $i` compiles
byte-identically in both arms and still measured +6.3%. `method-call.raku` and
`bench-class.raku` measured 0.0%.

## What still refuses, and what it turned up

Every control still dies loudly and with an unchanged message: a non-rw
attribute accessor, a raw-invocant routine that is not rw-capable, an rw-capable
routine whose invocant is not raw, a raw-invocant body that returns a value
rather than a location, `.self` (an ADR non-goal), and a computed invocant.

Two divergences the ADR's tables did not contain were measured and recorded as
tickets rather than folded in: an `is rw` **method** (as opposed to an attribute
accessor) still produces no container for a `:=` bind or an lvalue invocant, and
argument position still loses the container — where the `is raw` twin
(`sub f(\x) is raw { x }; f($c.v) = 9`) is *silently* wrong rather than a
refusal. The second of those also corrects a row in ADR-0067 itself, which cited
`sub g($y is rw) {...}; g($c.v)` as raku dying with "expects a writable
container": that is mutsu's diagnostic, not raku's — raku answers `9`.

Pinned by `t/lvalue-invocant-attribute-accessor-container.t` (25 tests) and
`t/lvalue-invocant-user-raw-method.t` (8 tests), both byte-identical under
`mutsu` and `raku`.
