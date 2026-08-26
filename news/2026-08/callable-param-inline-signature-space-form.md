# Parser accepts `&`-sigil parameters with a space-separated inline signature

`LibZip`'s `lib/LibZip/NativeCall.pm6` declares native subs that take a C
function pointer (callback) parameter, typed with an anonymous `Callable`
signature written with whitespace before the parens and a trailing `#`
comment on each parameter line, e.g.:

```raku
sub zip_source_function(zip                   # zip*
                       ,& (Pointer, Pointer, int64, int32 --> int64) # cb
                       ,Pointer                        # void*
                       ) is native(LIB) is export { * }
```

`use LibZip;` compiled cleanly under `raku` but failed under mutsu with
`expected ')'` partway through the signature. `TODO_roast/BLOCKERS.md` flagged
several possible triggers for this: the leading-comma style, the space
between `&` and `(`, and the trailing `#` comments.

## Root cause

Only one of the flagged constructs actually mattered: the space between `&`
and `(`. Bisecting minimal hand repros against real `raku` narrowed it down
to `sub f(& (Int --> Int)) { }` — an **anonymous** `&`-sigil parameter with
an inline code signature, separated from `&` by whitespace instead of the `:`
that `&:(...)` uses.

The **named** form of the same idiom, `sub f(&cb (Int --> Int)) { }`, already
parsed correctly — it falls through to the parser's generic "sub-signature
after variable" branch in `parse_single_param_inner`
(`src/parser/stmt/sub_param/param_inner.rs`), the same mechanism `$x ($a,
$b)` uses on other sigils. But the **anonymous** form never reached that
branch: an earlier "bare `&`" branch (for an anonymous callable parameter
with *no* attached signature, e.g. `sub f(&, $x)`) matched `&` followed by
whitespace and returned immediately, leaving the `(...)` unconsumed. The
outer signature parser then choked on the leftover `(...)` with a bare
"expected ')'".

Cross-checking against real `raku`'s own signature semantics
(`.signature.params[0].type` reports `Callable` for both the named and
anonymous forms; actually *calling* the bound callable through either form
dies with "Cannot unpack or Capture", because `(...)` after a `&`-sigil
parameter is a destructuring sub-signature, not a type constraint on the
callable's own signature) confirmed that reusing the same "sub-signature
after variable" path for the anonymous form is not just convenient but
semantically correct — it is the exact mechanism rakudo itself uses.

## Fix

`src/parser/stmt/sub_param/param_inner.rs`: before the "bare `&`" branch
short-circuits on `&` followed by whitespace, peek past the whitespace (and
any `#` comments — handled by the existing `ws()` helper) for a `(`. If
found, skip the bare-`&` branch and fall through to the general `var_name` +
"sub-signature after variable" path, which already handles the named form.
The leading-comma parameter-list style and the trailing `#` comments in the
original repro were both already handled correctly by the existing
`ws()`/param-list machinery and were red herrings.

## A second, related bug: wrong file/line on a parse error inside a `use`d module

While investigating, `use LibZip;` from `-e` initially reported
`at -e:498` — a line number belonging to the imported module's own source,
misattributed to the (much shorter) `-e` snippet. This turned out to be a
real, general diagnostics bug: `parse_module_source`
(`src/runtime/run_modules.rs`) parses each module file with its own source,
so the parser's line/column are correctly computed relative to *that*
module — but nothing recorded which file that was, so
`error_render::render_error` always fell back to whichever `source`/
`program_name` the CLI passed in for the top-level entry point. Since the
module's failing line number is usually out of range for the (often much
shorter) entry-point source, the `------>` source snippet was silently
dropped too, not just mislabeled.

Fixed by adding `source_file`/`source_text` fields to `RuntimeErrorCold`
(`src/value/error.rs`), populated by `parse_module_source` at the exact spot
where a module's own parse failure is caught, and consulted first by
`error_render::render_error` ahead of the CLI's entry-point source/name.

## Tests

- `t/callable-param-space-signature.t` — the anonymous and named
  space-separated `&`-sigil signature forms (including split across a
  newline, and the leading-comma/trailing-comment NativeCall style from the
  original repro), plus non-regression pins for the bare-`&` terminator
  forms (`&`, `&,`, `&?`, `&!`) and a negative case (garbage after the
  signature is still rejected). All assertions pass under both `raku` and
  mutsu.
- `t/use-module-parse-error-location.t` — spawns a real subprocess against a
  temp module with a genuine parse error, asserting the reported file name
  and line number are the module's, not the entry point's. Passes under
  both `raku` and mutsu.

## Status of `LibZip` itself

With this fix, `use LibZip;` (`-I` pointing at a locally reconstructed
minimal copy of the module) proceeds past the construct this ticket was
about. `LibZip` has 0 known dependents and a 2-assertion test suite, so
vendoring it as a bundled battery is not being pursued here — this PR is
scoped to the general parser/diagnostics fixes only.
