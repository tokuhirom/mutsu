# An ordinary indexed read stops walking the whole subscript funnel

`@a[$i]` cost **2,261 simulated instructions** — measured the way
[#8308](https://github.com/tokuhirom/mutsu/issues/8308) asks for, as the
difference between two 400,000-iteration loops identical apart from the
subscript, so the number is the read and nothing else. Rakudo's marginal cost
for the same read is ~39 ns; mutsu's was ~330 ns. On a read-dominated program
that was the whole gap: `benchmarks/bench-threads-serial.raku` performs 600,000
celled reads against 80,000 stores, so the reads alone were ~200 ms of a ~790 ms
run — comparable to the entire store half that
[#8069](https://github.com/tokuhirom/mutsu/issues/8069) and
[#8086](https://github.com/tokuhirom/mutsu/issues/8086) had already been spent
on.

## Where the instructions went

`exec_index_op_with_positional` is one ~2,200-line function serving every
subscript in the language. Before it reaches the `(Array, Int)` match arm that
actually reads an element, it walks a preamble that probes, in order, for: a
not-yet-run `.map`/`.grep` Seq index, an itemized index, a `Proxy` receiver, a
scalar-held Range, an empty `IO::Path::Parts` subscript, a Junction target, an
object-hash key type, a Junction key, a `HashEntryRef`, a `Scalar` wrapper, a
`ContainerRef` cell, an enum index, a `Bool` index, a user object with an `.Int`
method, a `Failure` target, a user `postcircumfix:<[ ]>` candidate, an unreified
`IO::Handle.lines` Seq, a `Seq` index, an itemized-array index, a Range key on a
Hash, a string index to numify, an object-hash lookup, a QuantHash coercion key,
and a `^parameterize` metaclass method — then falls into a match whose first
arms are the one-element-list rule and the Whatever/HyperWhatever slices. That
is some forty `Value::view()` calls, and `nanbox::peek::view_kind` alone
accounted for 434 instructions per read. The arm that finally answers then
builds a throwaway `Value::array_with_kind` purely to ask
`typed_container_default` for a default that an in-range read never uses.

None of that can change the answer for a plain non-negative `Int` into a plain
array. So it is now skipped rather than made cheaper.

## The fast path, and what it refuses

`Interpreter::index_fast_path_element` peeks the two operands and answers
directly only when every one of these holds: the subscript is `[...]`, the core
subscript routine's re-entry guard is clear, the index is an `Int` at least
zero, the target is a `List` or `Array` (not `Shaped`, `Lazy`, or itemized)
carrying no element type, no `is default(...)` and no ADR-0030 native backing,
the index is in range, and the element is itself an ordinary value rather than a
hole, a `:=`-bound cell or a deferred bind token. An associative twin does the
same for `%h{$k}` with a `Str` key that is *present* in a hash with no key type
— a present entry answers itself whatever metadata the hash carries, because
the element type, the default and `hash_autovivify` only ever decide what a
*miss* answers.

Everything else falls through to the general path untouched, which is the point:
the fast path adds no new definition of any edge case, and the stack is only
peeked, so a refusal costs two `view()` calls and leaves the operands exactly
where they were. `t/collections/subscript/index-read-fast-path.t` pins both
halves — the nine shapes it serves and the twenty-six it must decline, each one
checked against rakudo's answer.

## Result

Deterministic instruction counts (`scripts/bench-det.sh`'s metric, the one #8308
specifies because wall clock on this box has a 1.7x spread):

| marginal cost of one read | before | after |
| --- | ---: | ---: |
| `@a[$i]` | 2,261 Ir | 884 Ir |
| `%h{$k}` | 2,630 Ir | 1,443 Ir |

`benchmarks/bench-index-read.raku` as a whole: 8,950,340,031 → 7,855,032,763 Ir
with `MUTSU_JIT=off` (-12.2%), and 7,072,702,964 → 5,977,264,962 with the JIT on
(-15.5%). The remaining per-read cost is no longer in the subscript op: it is
`exec_get_local_op_inner` pushing the container (262 Ir), the two extra
interpreter dispatches (~290 Ir), and — on the hash side — SipHash (158 Ir).

In wall clock the marginal read now measures in the 35-60 ns range against
rakudo's ~40 ns on the same box, which is to say it is no longer separable from
run-to-run noise. `benchmarks/bench-index-read.raku` runs at ~0.6x rakudo.

## The benchmark that should have existed

`bench-index-store` closed half of the blind spot #8069 found — nothing in
`benchmarks/` wrote through a subscript. Nothing read through one either:
`array-ops` and `hash-access` measure `grep`/`map`/`elems` and whole-hash
lookup routines, not `@a[$i]` and `%h{$k}`. `benchmarks/bench-index-read.raku`
is the companion: the array form, the associative form, the chained
`@a[$i][$j]` form, and the `:=`-bound celled form that `bench-threads-serial`
actually performs. Like its store twin it deliberately uses the simplest shape
the read path has, so any cost it shows is cost the fast path is failing to
avoid — and it prints a checksum that matches rakudo's, so a wrong answer fails
it as loudly as a slow one.
