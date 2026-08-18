# `CBOR::Simple`'s own upstream suite fails broadly outside the narrow slice Cro::HTTP/Log::Timeline exercise

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
