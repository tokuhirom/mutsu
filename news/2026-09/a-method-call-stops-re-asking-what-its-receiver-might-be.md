# A method call stops re-asking what its receiver might be

`$o.m()` on a plain `class C { method m() {...} }` cost **21,379 instructions**. Rakudo does a
method call in roughly thirty cycles. [#8880](https://github.com/tokuhirom/mutsu/issues/8880) had
already established that the gap is not one slow step but a chain of speculative probes: before
anything dispatches, `CallMethodMut` asks, by name and from scratch, whether this receiver might be
a proto, an exception whose `Str` delegates to a user `message`, a lazy list, an attribute accessor,
an immutable scalar, an `IO::Handle`, an `IO::Path`, a CStruct, a class with a native method of this
name — and for an ordinary object every one of them answers "no", on every call, having recomputed
the same MRO walks and interned the same names to get there.

This is the cache **in front of** that chain. One set, keyed on `(receiver class, method name)`,
says "the whole prefix has been observed inert for this pair"; a hit skips it and goes straight to
the user-method dispatch. On the issue's own repro that is **−27.71% of the program's instructions**
and **−30.9% per method call**.

## Reaching the tail is the proof

The obvious way to build such a gate is to enumerate the conditions under which no probe can fire.
That list would be wrong the first week and rotten the first time someone adds a probe, because it
duplicates a decision the chain already makes and there is nothing to keep the copy honest.

So the memo is not derived from a list of conditions. Every probe in the chain *returns* when it
claims a call — there is no path to the dispatch tail that steps over one. A dispatch that arrives
at the tail has therefore been declined by all of them, and that arrival is where the memo is
written. The certificate is the control flow itself, which cannot drift from the probes because it
*is* the probes.

What the gate still has to do is pin everything the prefix reads that is not the key:

* the call shape — **zero arguments**, no `.^`/`.!` modifier, not quoted, no accessor-ref marker —
  is required identically on the install and on the replay, so no probe's argument-shaped early-out
  can differ between them;
* the registry (methods, roles, wraps, attributes, MRO) is pinned by `Registry::method_generation`,
  the same latch the sibling method caches already use — `.^add_method`, `augment`, a `.wrap` and a
  role application all bump it and drop the set;
* the three class families whose probes read the *instance* rather than the class are refused
  outright at install time: anything with `IO::Handle`/`IO::Path` in its MRO (the native handle probe
  keys on the live `handle` attribute), a CStruct class (whose fields live in native memory rather
  than the attribute map), and any class the program did not declare itself.

A stale entry could only ever cost a skipped probe, never a wrong dispatch: the lane runs the
identical `resolve_method_cached` → wrap-chain → `call_compiled_method` tail the full path ends in —
the same code, reached by a second entry rather than a copy — and falls back to the whole path
unchanged if that tail declines. That is the difference from the attempt reverted in
`46cba5f1`, which swapped the *dispatch mechanics* for `call_compiled_method_fast` and lost
container aliasing for sigilless and raw parameters. Nothing about how the method is called changes
here; only the questions asked before it.

## Measured

`class C { has $.v; method m() { 1 } }` called 100 000 times in a `while` loop, `--profile
profiling` under callgrind, with the bare loop skeleton (244,965,161 Ir) subtracted:

| | before | after | |
| --- | ---: | ---: | ---: |
| program total | 2,382,859,098 | 1,722,666,685 | **−27.71%** |
| per method call | 21,379 | 14,777 | **−30.9%** |

Where the 6,602 instructions went:

| | before | after | |
| --- | ---: | ---: | ---: |
| `LocalKey<T>::with` (thread-local access) | 219,935,596 | 117,337,416 | −46.6% |
| `Registry::class_mro_readonly` | 80,501,566 | 16,102,371 | −80.0% |
| `resolve_user_method_or_accessor` | 43,800,000 | 0 | −100% |
| `is_native_method` | 36,300,000 | 0 | −100% |
| `StrSearcher::new` | 21,310,320 | 0 | −100% |
| `try_fast_accessor_read` | 10,700,025 | 0 | −100% |
| `try_proto_method_body` | 9,100,043 | 0 | −100% |
| `Symbol::resolve` | 12,300,246 | 3,900,312 | −68.3% |
| `malloc` / `free` / `_int_free` | 178,836,437 | 124,988,259 | −30.1% |

The four entries that go to zero are the point: they are not cheaper, they are not called. `StrSearcher::new`
is there because the probes ask `contains("::")` about names they re-derive; `class_mro_readonly` because
four separate probes each walked the MRO to answer a question about a class that had not changed
since the program started.

## On the tracked benchmarks

A microbenchmark is not a verdict, so the same change measured by `scripts/bench-det.sh` against
`origin/main` (both release, both lanes, instructions and allocator calls):

| | Ir | | allocations | |
| --- | ---: | ---: | ---: | ---: |
| `bench-class` | 1,226,271,303 → 1,088,627,535 | **−11.22%** | 979,440 → 903,462 | **−7.76%** |
| `bench-class+jit` | 1,217,813,439 → 1,081,359,386 | **−11.20%** | 985,893 → 909,903 | −7.71% |
| `bench-ctor` | 1,577,898,741 → 1,579,478,093 | **+0.10%** | 1,530,871 → 1,530,885 | ±0.00% |
| `bench-ctor+jit` | 1,570,825,916 → 1,574,591,760 | **+0.24%** | — | ±0.00% |
| `bench-json-fast` | 2,688,025,124 → 2,680,365,147 | −0.28% | — | ±0.00% |
| `bench-grammar-parse` | 28,446,064 → 28,418,917 | −0.10% | — | ±0.00% |

Two things in that table are worth saying out loud. `bench-ctor` gets **slightly slower**: it is
`C.new(...)` on a bareword, a `Package` receiver the lane never applies to, so all it sees is the
gate's own cost — one view match and one cleared field per dispatch. That is the price of the
mechanism where it does not pay, and it is what it costs. And `bench-json-fast`, the benchmark that
motivated #8880 in the first place, barely moves: `JSON::Fast`'s hot calls mostly carry arguments,
which the zero-argument gate excludes. Widening the key to cover arguments is the obvious next step
and is deliberately not taken here — excluding them is exactly what makes the argument-shaped
early-outs in the probe chain impossible to get wrong.

## What this does not do

The lane covers the `CallMethodMut` opcode — a method call on a *named* receiver, which is the
shape `$o.m()` compiles to and the one the issue measures. The plain `CallMethod` opcode has its own
probe prefix and its own `fast_method_cache` wiring and is untouched.

The numbers above are local A/B runs against `origin/main`, taken minutes apart on the same box;
`bench-history.tsv` on `bench-data` is the tracked series and is what any document should cite.

[#8880](https://github.com/tokuhirom/mutsu/issues/8880) stays open. A method call is still 14,777
instructions against rakudo's thirty cycles, and the remaining budget is now dominated by the
dispatch itself — `call_compiled_method_fast`, the per-call `attributes.to_map()`, and
`push_method_dispatch_frame` — rather than by asking what the receiver is.
