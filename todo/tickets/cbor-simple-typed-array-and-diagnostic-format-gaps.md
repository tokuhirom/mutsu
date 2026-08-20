# `CBOR::Simple`'s own upstream suite fails broadly outside the narrow slice Cro::HTTP/Log::Timeline exercise

> **Update (2026-08-20, second pass): test 68's nested-array decode aliasing bug (and the stack
> overflow right after it) are ROOT-CAUSED and FIXED.** `01-basic.rakutest` now runs cleanly to
> completion (74/74 attempted, only the 4 pre-existing unrelated float/unicode failures at tests
> 24/27/28/53 remain — see the earlier 2026-08-18 update).
>
> **Root cause**: `cell_store_preserving_container_identity` (`vm_var_assign_ops.rs`) mutates an
> existing `ContainerRef` cell's backing container IN PLACE when a whole array/hash reassignment
> reaches it, so aliases (`my @b := @a; @a = ...`) observe the update — correct for a REAL
> reassignment. But anonymous containers (`my @ = EXPR`, `my % = EXPR`) all compile to a single
> shared slot name (`@__ANON_ARRAY__` / `%__ANON_HASH__`; see the `is_anon_container` comment in
> `vm_var_assign_set_local.rs`), so each successive declaration is a DISTINCT logical variable that
> merely reuses that name — and every OTHER in-place-reassign call site already excludes anon names
> for exactly this reason (`vm_var_assign_set_local.rs`'s `SetLocal` handler, `vm_exec_dispatch.rs`'s
> `SetGlobal` handler, `vm_misc_assign.rs`'s array/hash guards). `cell_store_preserving_container_identity`
> was the one call site missing that exclusion. It only matters when the anonymous slot has been
> promoted to a `ContainerRef` cell by escape analysis (a captured-and-mutated outer container) —
> which is exactly `CBOR::Simple`'s `decode-array` shape: `!! my @ = (^read-uint).map(&decode)` is a
> closure declared and invoked fresh per recursive `cbor-decode()` call, and `&decode` reading a
> captured free variable is what triggers the promotion. Two sibling recursive calls (one per nested
> definite-length array inside an indefinite-length one) then landed on the SAME cell, so the second
> call's in-place reassign silently overwrote the first's still-referenced result — exactly the
> `$[1, [4, 5], [4, 5]]` symptom, and the stack overflow shortly after was a downstream consequence
> of decoding against corrupted state.
>
> **Fix**: threaded the variable name through `cell_store_preserving_container_identity`'s 7 call
> sites and added the same `name.contains("__ANON")` exclusion its four siblings already have.
> Pinned by `t/anon-container-cell-inplace-reassign.t` (a CBOR-independent mimic of the shape,
> plus the actual CBOR::Simple repro and a second nested-indefinite case). Full `t/` suite (30219
> tests), the map/grep/reduce/first/splice/state/closure/container roast files (91 files, ~3450
> tests), and `Text::CSV`'s `79_callbacks.t` (the other file the surrounding code comments call out
> as a past regression risk for this exact mechanism) all pass with no behavior change elsewhere.
>
> Two standalone (non-CBOR) mimics of the bug shape were tried during the investigation and neither
> reproduced it even *before* the fix — the trigger needs `CBOR::Simple`'s specific closure/escape
> shape (many free variables captured across `@decoders`'s dispatch table, `$pos`/`@bytes`/`$breakable`),
> not just "closure declared fresh per recursive call, returns a naked `my @ = ...`". Not fully
> reduced past that; not needed once the root cause was found via `MUTSU_DEBUG_ARRAY_ALLOC`-style
> instrumented builds tracing every `array_inplace_reassign` call site rather than more guessing.

> **Update (2026-08-20): test 68's nested-array decode aliasing bug reduced to a 4-line,
> CBOR::Simple-scoped repro (previously thought to need the full file's accumulated state) —
> root cause still open, filed as a lead for a follow-up `todo/deep`.** Bisecting the file
> (binary-search over which earlier subtests are prerequisites, see method below) found the
> trigger has nothing to do with "68 subtests of accumulated state": it needs exactly one prior
> `cbor-encode()` call on an inline, expression-position `my @ = ...` array, of ANY length
> (including a single element), followed by decoding an indefinite-length array whose two
> elements are themselves definite-length nested arrays:
>
> ```raku
> use CBOR::Simple;
> sub hex-decode(Str:D $hex, $buf-type = buf8) { $buf-type.new($hex.comb(2).map(*.parse-base(16))) }
> say cbor-encode((my @ = 1,2,3));                       # warm-up; ANY length reproduces, even 1
> say cbor-decode(hex-decode('9f01820203820405ff'));     # expect [1, [2, 3], [4, 5]]
> # mutsu: [1 [4 5] [4 5]]  -- second nested array clobbers the first
> ```
>
> Isolating variables (each tested independently, release-independent — `MUTSU_JIT=off` still
> reproduces, so this is a base-interpreter bug, not a JIT artifact):
>
> - The warm-up call must be `cbor-encode((my @ = ...))` — an **inline, expression-position**
>   `my @ = LIST` used directly as a call argument. `my @a = 1,2,3; cbor-encode(@a)` (declared as
>   its own statement first, then passed by name) does **not** trigger it.
> - The warm-up must be an `encode` call specifically; decoding the same values first (without
>   ever encoding) does not trigger it, even decoding both the nested array AND a 25-element
>   array beforehand.
> - Array length of the warm-up value is irrelevant — `1` element already triggers it, `[1]`
>   literal (not `my @ = ...`) does not.
> - The corruption itself: `CBOR::Simple`'s `decode-array` (`Simple.rakumod:690-701`) has
>   `!! my @ = (^read-uint).map(&decode)` — the exact same **inline `my @ = EXPR` as an
>   expression-position ternary branch** shape as the warm-up. This closure is freshly declared
>   (`my &decode-array = { ... }`) inside `multi cbor-decode(...)`, which is called recursively
>   once per indefinite-array element — so the `[2, 3]` and `[4, 5]` sub-arrays are each decoded
>   in **separate, sibling recursive `cbor-decode()` invocations**, yet the returned array
>   containers end up **aliased to the same object** (the second overwrites the first, visible in
>   the first's still-held reference).
>
> **Not yet root-caused**: two attempts to reproduce with a CBOR-independent script (a hand-rolled
> recursive decoder with the same "closure declared fresh per recursive call, returns an inline
> `my @ = ...` from a ternary branch, results collected into an outer array" shape) both decoded
> correctly — so the trigger needs something more specific to `CBOR::Simple`'s actual code
> (candidates not yet tested: the `.map(&decode)` where `&decode` is itself a `my &decode = {...}`
> dispatching through the `@decoders` table at `Simple.rakumod:1191`/`1201`, or some interaction
> specific to how `read-uint` advances `$pos` across the recursive calls). This is a **silent data
> corruption** bug (no crash, no warning — just a wrong value), so it is a good candidate for its
> own `todo/deep` ticket once someone reduces the CBOR-independent repro; the natural next step is
> `rust-gdb` on `my @ = (^read-uint).map(&decode)`'s container-allocation site (CLAUDE.md's
> "Debugging guidelines" — break where an anonymous/expression-position array `VarDecl` allocates
> its container, compare pointer identity between the two decode-array invocations) rather than
> more guess-driven raku-level probing.
>
> **Bisection method used** (reusable for other "needs full accumulated state" style bugs): start
> from the failing full file, binary-search by commenting out/keeping line ranges (front half vs
> back half of the file, re-test, recurse into whichever half still reproduces) rather than
> removing tests one at a time from the front. Converged in ~6 rounds from "needs all 68 prior
> subtests" to "needs exactly 2 specific lines" (`t/01-basic.rakutest:113` + `:114`), then further
> reduced those two lines' *shape* (not their literal content) by substitution.

> **Update (2026-08-19): the `01-basic.rakutest` "No matching candidates for proto sub: matches"
> failure (test 12) is NOT a CBOR::Simple bug at all — root-caused and reduced to a 6-line,
> CBOR-independent repro.** The vendored test file's own `use lib $*PROGRAM.sibling('lib');`
> (a computed, non-literal `use lib` argument, used to find its sibling `CodecMatches.rakumod`)
> defers that module's declarations from becoming visible to mutsu's PARSER until mainline
> execution reaches the `use` statement — unlike `-I` or a literal `use lib 'path'`, whose
> declarations ARE visible to the parser before it processes later statements in the same file.
> By the time the parser reaches `matches -18446744073709551616, '...'` (a bareword call to a
> multi sub whose declaration it does not yet statically know about), the negative-number first
> argument's leading `-` misparses, splitting the call into a 1-arg statement plus a dangling
> string literal — which is exactly why every failing `matches` call in this file has a NEGATIVE
> first argument and every passing one is positive. Full mechanism, the isolating 2x2 test
> matrix, and why this needs a `todo/deep` (not a quick fix) are in
> `todo/deep/use-lib-dynamic-path-defers-declaration-visibility-to-parser.md`. **Re-running the
> CBOR::Simple suite with an explicit `-I <path-to-t/lib>` instead of letting the file's own
> `use lib $*PROGRAM.sibling('lib')` resolve it should route around this bug** and let the
> `01`/`03`/`04`/`06` triage below continue on real per-file issues, if any remain, without this
> noise. Not attempted this session — the next session picking this up should start there.

## Symptom

Bundling `CBOR::Simple` (a `Log::Timeline` → `Cro::HTTP` dependency, see
`docs/batteries/cro-deps.md`) and running its own upstream test suite via
`scripts/battery-testsuite.sh` shows most files failing, e.g.:

```
$ mutsu -I modules/CBOR-Simple/lib -I modules/TinyFloats/lib t/06-typed-arrays.rakutest
array cannot be parameterized
  in sub cbor-decode at .../CBOR/Simple.rakumod line 594
```

`00-use.rakutest` (1/1) is the only fully-passing file; `01-basic.rakutest`,
`02-malformed.rakutest`, `03-diagnostic.rakutest`, `04-tags.rakutest`,
`05-malformed-tags.rakutest`, `06-typed-arrays.rakutest` all fail heavily
(see `tmp/battery-update.log` from the 2026-08-13 Cro-bundling session for
the exact counts).

## Why this does not block the Cro battery

`Cro::HTTP` (35/35) and `Log::Timeline`'s own `has-output.rakutest` pass
cleanly — the CBOR surface Cro actually exercises (via `Log::Timeline`'s
CBOR-sequence output backend) is narrow, per
`docs/batteries/cro-http.md`'s "Cro reachability" notes: "the buf read/write
family only runs when log outputs are activated". `06-typed-arrays.rakutest`
needs `array[int32]`-style parameterized array types (`"array cannot be
parameterized"`), which is a general mutsu gap, not CBOR::Simple-specific.

## Root cause (not yet diagnosed per-file)

At least two distinct gaps bundled together in the failures:

1. Parameterized/typed `array` types (`array[uint8]` etc.) — general
   language feature gap, affects `06-typed-arrays.rakutest` at minimum.
2. Unclear for `01-basic`/`02-malformed`/`03-diagnostic`/`04-tags`/
   `05-malformed-tags` — needs the standard investigation procedure
   (run under `raku` first, `--dump-ast` the failing assertion, compare).

## Next steps

1. Run each failing file under `raku` to establish the expected baseline.
2. `--dump-ast` the `array[...]`-parameterized declarations in
   `06-typed-arrays.rakutest` to see what mutsu's parser/typechecker does
   with them.
3. Triage `01`–`05` individually — likely several unrelated general bugs
   bundled into one ticket; split into separate tickets once diagnosed.

## Update (2026-08-14): forward-captured-code-var-snapshot fixed, one more
## gap found in `01`

`todo/tickets/forward-captured-code-var-snapshot.md` (a `&`-sigil lexical
read bare, e.g. `.map(&decode)`, before its own `my &decode = ...`
declaration compiles — the exact shape `cbor-decode`'s `decode-array` /
`decode-map` / `decode-tag` use to call the not-yet-declared `&decode`) is
now fixed (`news/2026-08/forward-captured-code-var-snapshot.md`). Manually
verified with `mutsu -I modules/CBOR-Simple/lib`: int/string/array/nested-map
`cbor-encode` → `cbor-decode` round-trips now work correctly, including the
mutually-recursive `decode-array`/`decode-map`/`decode` dispatch that was
previously blocked ("Cannot map a Nil to a Range").

The full round-trip (all of `Simple.rakumod`'s decode paths) is still blocked
by a **separate, unrelated** parser bug: `decode-sval` (`Simple.rakumod:734`)
opens with `my constant %svals = 20 => False, 21 => True, 22 => Any, 23 =>
Mu;` immediately followed by an `if / elsif / elsif ... / else` chain, and
mutsu's parser fails the first `elsif` with `Undeclared routine: elsif used`.
Minimal repro (nothing CBOR-specific — reproduces standalone):

```raku
if 1 == 1 {
    my constant %svals = 20 => False, 21 => True;
    if 1 == 2 { say "a" }
    elsif 1 == 1 { say "b" }
}
```

Expected (raku): `b`. mutsu: parse error, `elsif` misparsed as a bareword
function call. Removing the `my constant ...;` line makes the identical
`if`/`elsif`/`else` chain parse fine, so the trigger is specifically a `my
constant` declaration statement immediately preceding a following
`if`/`elsif` at the same block nesting level. Not yet root-caused past that;
next step is `--dump-ast` on the minimal repro to see how the parser's
statement-boundary detection after `my constant` differs from after a plain
`my`.

## Update (2026-08-18): the `my constant` + `elsif` parser bug is fixed

The minimal repro from the 2026-08-14 update above is fixed
(`src/parser/stmt/decl/constant_subset.rs`): `constant_decl` was eagerly
consuming its own trailing `;` before returning, so when
`my_decl_dispatch.rs`'s "my constant" branch applied
`parse_statement_modifier` to the remainder, that function's own "if there's
a semicolon, the statement is already terminated" check never fired (the
`;` was already gone) — it instead misparsed the immediately-following
`if`/`elsif`/... statement as a dangling statement modifier on the constant
declaration. Fixed by leaving the `;` for the caller (matching how a plain
`my $x = 5;` already behaves); pinned by
`t/constant-elsif-statement-boundary.t`. `decode-sval`'s `if`/`elsif` chain
in `Simple.rakumod:734` should now parse correctly — not yet re-verified
against the full round-trip or the `06-typed-arrays.rakutest` /
`01`-`05` file triage below, which remain open.

(A related but separate bug was found and filed apart from this one:
`todo/tickets/constant-statement-modifier-value-lost.md` — a GENUINE
statement modifier on `constant`, e.g. `my constant $w = 11 if True;`, parses
fine but the bound value is lost, `$w` reads back `Any` instead of `11`.)

## Reproduce

```sh
git clone https://github.com/japhb/CBOR-Simple.git /tmp/cbor-simple
cd /tmp/cbor-simple && git checkout 0.1.4
timeout 20 mutsu -I /path/to/mutsu/modules/CBOR-Simple/lib -I /path/to/mutsu/modules/TinyFloats/lib t/06-typed-arrays.rakutest
```

## Update (2026-08-18): root-caused and fixed the "No matching candidates for proto sub: matches" fatal in `01`/`04`

That symptom was a misleading downstream effect, not a real multi-dispatch
bug: `CBOR::Simple`'s own integer-boundary constants
(`enum CBORMinMax (... CBOR_Min_NInt_63Bit => -9223372036854775808, ...)`)
made `cbor-encode`'s `CBOR_Min_NInt_63Bit <= $_ <= CBOR_Max_UInt_63Bit`
chained comparison wrongly reject every negative integer, so `cbor-encode`
produced the wrong CBOR blob for any negative value (tag-3 BigInt instead of
compact native encoding) — a general interpreter bug (`arith_negate` not
downcasting a BigInt negation back to `Int` when it lands in `i64` range,
combined with `EnumValue::as_i64` returning `0` for the resulting
BigInt-backed `Generic` enum variant), unrelated to `CBOR::Simple` itself.
Fixed in `news/2026-08/bigint-negate-i64-min-downcast.md`. The "No matching
candidates" error was `CodecMatches.rakumod`'s own `matches` multi silently
running out of test count/exiting oddly after enough subtest assertions
failed earlier in the file — a real symptom, but downstream of the encoding
bug, not a proto-dispatch bug at all.

**Re-measured after the fix:**

- `01-basic.rakutest`: now runs to test 68/74 (previously died at test 12) —
  1-67 pass; test 68 fails on a **different, new** bug: `cbor-decode` of a
  nested-array structure (`9F01820203820405FF`) returns
  `$[1, [4, 5], [4, 5]]` instead of `$[1, [2, 3], [4, 5]]` — looks like a
  decoded sub-array aliasing/caching bug (the first nested array's decoded
  value gets overwritten by the second). The run then hits a **stack
  overflow** shortly after (`thread '<unknown>' has overflowed its stack`) —
  needs its own investigation, not yet started.
- `02-malformed.rakutest`, `05-malformed-tags.rakutest`: already mostly
  passing before this fix (94/94 and 23/23 with only 1-2 stray failures each
  per a fresh run) — not re-verified whether the encoding fix changed
  anything here, likely unrelated (malformed-input tests don't exercise
  valid negative-integer encoding).
- `03-diagnostic.rakutest`, `04-tags.rakutest`, `06-typed-arrays.rakutest`:
  not re-triaged this round.

**Next steps for whoever picks this up:** reduce the `01-basic.rakutest`
nested-array decode aliasing bug (test 68) and the stack overflow into a
standalone repro (not yet attempted) before touching `03`/`04`/`06`.

## Update (2026-08-19): the `01-basic.rakutest` blocker before test 68 was a general parser bug, not CBOR::Simple's — worked around, test 68's bug confirmed still real but is STATE-dependent

The "No matching candidates for proto sub: matches" failure that stopped the file at test 12 is a
general `use lib`-with-a-computed-argument parser bug, unrelated to CBOR::Simple — full
root-cause and a 6-line standalone repro are in
`todo/deep/use-lib-dynamic-path-defers-declaration-visibility-to-parser.md`. Running the file with
an explicit `-I <path-to-t/lib>` (bypassing its own `use lib $*PROGRAM.sibling('lib');`) routes
around it: `mutsu -I modules/CBOR-Simple/lib -I modules/TinyFloats/lib -I
<cbor-simple-checkout>/t/lib <cbor-simple-checkout>/t/01-basic.rakutest` now reaches **test 68/74**
(1-67 pass), reproducing exactly the nested-array decode aliasing bug + stack overflow this
ticket's 2026-08-18 update already found.

**Important correction for whoever reduces test 68 next:** decoding the SAME CBOR blob
(`9F01820203820405FF`) in ISOLATION (a fresh `use CBOR::Simple; cbor-decode(Buf.new(0x9F, 0x01,
0x82, 0x02, 0x03, 0x82, 0x04, 0x05, 0xFF))` with nothing else in the program) produces the
CORRECT `$[1, [2, 3], [4, 5]]` on current `main` — verified directly. The bug only reproduces
after the other 67 subtests' worth of accumulated program state (many prior `cbor-encode`/
`cbor-decode` calls, `matches` multi-dispatch resolutions, etc.) — i.e. it is NOT simply "decoding
this exact blob is broken", it needs a warm/stale cache or similar accumulated state to trigger.
Reducing it will need trimming the FULL file from the top (keeping all/most prior subtests) rather
than isolating just the failing blob, or bisecting which EARLIER subtest(s) are the prerequisite —
neither attempted yet.
