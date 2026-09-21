# `bench-index-store` was never mostly about `@a[$i] = $v`

`bench-index-store` has been the suite's only losing row for a while, sitting at
1.19-1.23x rakudo while its read-side twin `bench-index-read` wins at 0.83x. The
obvious reading is that the positional element store is still slow — it is the
form [#8069](https://github.com/tokuhirom/mutsu/issues/8069) was filed about, the
form the benchmark runs 500,000 times, and the form two rounds of that issue
([#8107](https://github.com/tokuhirom/mutsu/issues/8107),
[#8151](https://github.com/tokuhirom/mutsu/issues/8151)) have already been spent
on.

It is wrong. Decomposing the benchmark by section — differenced callgrind runs of
each store loop against the identical loop without the store, `--profile
profiling`, `MUTSU_JIT=off`, N against 2N so nothing in the difference is startup
or compilation:

| form | Ir/store | iterations | total |
| --- | ---: | ---: | ---: |
| `@a[$i] = $v` | 1,115 | 500,000 | 557 M |
| `%h{$k} = $v` | 3,642 | 200,000 | 728 M |
| `@a[$i][$j] = $v` | 12,134 | 100,000 | 1,213 M |

The form with five times the iterations was **22%** of the store cost. The two
forms nobody had touched were 78%, and the chained store alone — a fifth of the
iterations — was half.

That is the whole finding, and it is a benchmark-decomposition finding rather
than a profiling one: every previous round read the profile of the *array* loop,
where the array store is of course what dominates. The three sections share one
row, so the row could not say which one moved, which is exactly the objection
[#8086](https://github.com/tokuhirom/mutsu/issues/8086) raised about
`bench-threads` before it was given a single-threaded twin.

## What the two untouched forms were paying for

Both turned out to be shapes #8069 had already diagnosed and fixed once, for the
positional store only.

**The chained store rendered a refusal message it almost never returns.** #7556
C2 refuses a store into one of a `List`/`ItemList`'s bare elements, naming the
List: `Cannot modify an immutable List ((1 2))`. Building that message needs the
container, and the container is behind the mutable borrow the store's
`with_array_mut` closure takes out — so both call sites computed
`value_type_name` and `gist_value` on the line *above* the closure, where the
borrow had not started yet. Correct, and it made every chained store pay for an
exception that fires essentially never: `gist_value` renders each element of the
addressed container and joins them, which for the benchmark's eight-element rows
is one `gist_value`, eight `to_string_value` calls and a joined `String` per
store. 20,000 stores measured 20,000 `gist_value` calls and 160,000
`to_string_value` calls.

`immutable_list_refusal` builds it from the backing node and the kind *inside*
the closure instead, on the branch that returns it. The reconstruction is exact
rather than approximate — an `Array` value *is* the `(Gc<ArrayData>, ArrayKind)`
pair, `value_type_name` reads only the kind and `gist_value` only the node — so
the message is byte-identical at both the two-level and the three-or-more-level
site, which is what `t/vm/binding/nested-element-store-fast-lane.t` now pins by
wording rather than by "it throws".

**The associative lane was in the position the positional lane escaped from.**
`try_fast_hash_element_assign` is the oldest of the element-store fast paths, and
it had sat at the *bottom* of `exec_index_assign_expr_named_op`'s dispatch chain
since it was written. That is the exact position #8151 moved the positional lane
out of, where running it first — not making it cheaper — took a store from 3,097
instructions to 1,210. `try_fast_hash_element_assign_early` is the line-by-line
twin of that wrapper, establishing on its own the facts the preamble would
otherwise establish for it, and the two deliberately keep the same guard order so
that a future preamble step is obviously missing from both or from neither.

One of those facts was not previously the lane's to check. A `Proxy` element
mediates its own store, and the preamble's `existing_element_container` probe
caught that before the lane was ever reached; consulted first, the lane has to
refuse a `Proxy`/`VarRef` destination itself.

**The chained store had no fast lane at all.** ADR-0068 §4 step 3 treats it as its
own funnel, and `exec_index_assign_expr_nested_op_body` ran in full on every
write: it copies the variable name out of the constant pool, resolves a type
constraint by name, probes env by name for a user-object root, a `Pair` root and
a vivification-needed root, scans `code.locals` by name twice (once to nil the
slot, once to restore it), and re-interns the name at each env probe — **seven
`Symbol::intern` calls per store**, where the positional lane makes none. Then it
renders *both* subscripts to a decimal `String` with `to_string_value` and
immediately parses each one back with `str::parse::<usize>()`, for indices that
arrived on the stack as `Int`.

None of those questions is a property of the store. They are properties of the
two containers, and #8069 §2's answer applies unchanged: ask the containers.
`try_fast_nested_element_assign` reads `ArrayData::has_type_meta`, `ArrayKind`,
`HashData::has_type_meta` and the monotonic `env::*_possible()` latches, and
serves all four bracket combinations (`@a[i][j]`, `@a[i]<k>`, `%h<k>[i]`,
`%h<a><b>`).

## Measured

Same methodology, one base, minutes apart:

| form | before | after | |
| --- | ---: | ---: | --- |
| `@a[$i] = $v` | 1,115 | 1,110 | unchanged — the control |
| `%h{$k} = $v` | 3,642 | **2,144** | -41% |
| `@a[$i][$j] = $v` | 12,134 | **2,019** | -83% |

Over the benchmark's own iteration counts that is 6,688 M instructions of loop
body down to 5,375 M, **-19.6%**. The array row moving by 5 instructions out of
1,115 is the useful part of that table: it is the untouched control, and it says
the difference is in the two lanes and not in the measurement.

Instructions are not time. As a reading rather than as evidence, a paired
seven-run wall clock on this container puts `bench-index-store` at **0.79x**
rakudo (mutsu 0.669s median, raku 0.850s), against the 1.19-1.23x the row has
been sitting at — same checksum on both. Local wall clock drifts by more than
most single wins, so `bench-history.tsv` remains the authority on the row
itself; a ratio flip of that size is quoted here only because it is far outside
that drift.

## Two divergences found while pinning it

Neither is reachable by the new lane — `Nil` is absent from its rvalue allow-list
and a `Proxy` is on its destination reject-list, so it declines in both cases and
the unchanged body runs — so they are filed rather than folded in, and pinned as
`todo` in the new test:

- a chained store does not fire a `Proxy` element's `STORE`
  ([#8965](https://github.com/tokuhirom/mutsu/issues/8965));
- `Nil` does not decay to the container default there, contrary to ADR-0049
  ([#8966](https://github.com/tokuhirom/mutsu/issues/8966)).

## What this does not do

It does not touch #8069 §4.1's resolved container descriptor, and the three lanes
are still fast paths *beside* the name-keyed one — the shape §4.1 warns grows its
probes back one feature at a time. It adds two more of them, which is worth
saying plainly: the argument for doing it anyway is that the chained store had
none at all and was eleven times the positional store's cost, and that ADR-0097's
own slices are currently blocked on a closure-capture design question (§13) that
this work does not help with. §4.2-§4.5, the concurrent half, are untouched as
always; all three lanes stand down entirely once a second mutator thread exists.
