# `CBOR::Simple`'s own upstream suite fails broadly outside the narrow slice Cro::HTTP/Log::Timeline exercise

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
