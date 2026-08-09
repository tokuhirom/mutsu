# `*@` slurpy over-flattens nested `[...]` literals (inner Array elements are not treated as containerized)

## Affected tests

- `t/http2-request-serializer.rakutest` subtests 3 and 6 (both named "check 1"): the test sub is `sub test($request, $count, $desc, *@checks, ...)` called with `[[check1..check4], [check1..check4]]`. Under mutsu `@checks` becomes the 8 individual check callables instead of 2 groups of 4, so `@checks[$counter]` is a *single callable* per frame — frame 2 of "Header + Data" runs `*.flags == 5`-style checks against the wrong frame and fails. It also silently reduces coverage everywhere in this file: each frame runs only "check 1" instead of its full 4-check group (visible in the TAP: one `check 1` per frame, no `check 2..4`).
- `t/http2-response-serializer.rakutest` uses the same `*@checks` shape; its checks are currently unreachable (see `http2-supply-done-return-escapes-method.md`) but will hit this bug the moment that ticket is fixed.

## Repro

```raku
sub f(*@c) { say @c.elems; say @c[0].raku; say @c[1].raku }
f [[1,2],[3,4]];
```

- mutsu: `4` / `1` / `2` (fully flattened)
- raku: `2` / `$[1, 2]` / `$[3, 4]` (one level flattened; inner Arrays preserved as itemized elements)

Related probe results (mutsu vs raku):

| Call | mutsu `@c.elems` | raku |
|---|---|---|
| `f [[1,2],[3,4]]` | 4 | 2 |
| `my @x = [1,2],[3,4]; f @x` | 4 | 2 |
| `f [$[1,2],$[3,4]]` (explicit itemization) | 2 | 2 |
| `f [1,2]` | 2 | 2 |
| `my $a = [[1,2],[3,4]]; f $a` | 1 | 1 |

Underlying divergence: `[[1,2],[3,4]][0].raku` prints `[1, 2]` under mutsu but `$[1, 2]` under raku — mutsu does not model the Scalar container that every Array *element* lives in.

## Root cause

`flatten_value_for_slurpy` (`src/vm/vm_value_helpers.rs:548-554`) recurses into any `ValueView::Array` whose `ArrayKind` is not itemized (`ItemList`/`ItemArray`, see `src/value/value_collections.rs:18-20`). The function's contract ("itemized containers are preserved") is right, but the *inputs* never carry the itemization: an inner `[...]` literal element is stored as `ArrayKind::Array` (bare), because mutsu only marks itemization on explicit `$[...]` or scalar assignment — it does not model the rule that **elements of an Array are held in Scalar containers** and therefore never flatten further.

In Raku the slurpy flattening rule is: expand Iterables recursively, but stop at anything inside a Scalar container. Since every element of a real Array is containerized, expanding an Array must contribute its elements *as-is* (one level); only List-kind (uncontainerized) elements keep flattening.

## Fix direction

Narrow, semantics-first fix in `flatten_value_for_slurpy` (`src/vm/vm_value_helpers.rs:548`): when the value being expanded is a **real Array** (`kind.is_real_array()`, i.e. `Array`/`ItemArray`/`Shaped`/`Lazy` — the `[...]` constructor and `@`-vars), push its elements without recursing into element Arrays; keep full recursion only for `List`-kind values (`(1,(2,3))` must still flatten to 3). Concretely: expand one level for Array-kind, recurse per-element only when the element is a non-itemized `List`.

Cross-check cases the fix must keep correct (all verified against raku):

- `my @x = 1,2; f @x` → 2 (unchanged)
- `f (1,(2,3))` → 3 (List recursion preserved)
- `f [1,[2,3]]` → 2 (raku: `1, $[2,3]`)
- `f $a` where `$a = [[..],[..]]` → 1 (itemized scalar: unchanged)

Also audit the two other callers of the helper (`vm_value_helpers.rs:602`, `vm_exec_dispatch.rs:1905`) — they feed the same slurpy semantics and should inherit the fix.

The deeper alternative — itemizing elements at `MakeArray` (`src/compiler/expr.rs:290`) so `[0].raku` prints `$[1, 2]` — is the "correct architecture" endpoint but has a much larger blast radius (every `.raku` output, `is-deeply` comparisons); do it only with roast coverage, as a separate change.

## Verification

- `raku`-vs-mutsu on the probe table above (all five rows).
- `t/http2-request-serializer.rakutest`: subtests currently named "check 1" become the full 4-check groups; the file's TAP shows `check 1..check 4` per frame and 0 failures. Expect the file's total test count to GROW (each group's checks 2-4 start running) — update expectations accordingly, don't be surprised by a different plan count.
- New pin `t/slurpy-nested-array.t` with the probe table.
- `make test` — slurpy flattening is load-bearing everywhere; the roast run on CI is the real safety net (S06-signature tests).
