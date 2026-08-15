# A stored regex keeps its defining scope, and `<$var>` no longer leaks captures

A `Regex` value in Raku behaves like a closure: it can interpolate lexicals
from the scope it was written in, and when it is itself interpolated into
another pattern (`<$var>`, bare `$var`, `<@var>`) it matches as an isolated
sub-match — its own capture groups do not become part of the outer match's
capture numbering. mutsu got both of these wrong, in two unrelated ways that
happened to live in the same corner of the regex engine (the tokenizer /
interpolator that turns a stored `Regex` value back into pattern text or an
atom).

## Bug 1: the closure half

```raku
sub make() {
    my $word = 'abc';
    return rx/ $word /;
}
my $r = make();
say ("xx abc yy" ~~ $r).defined;   # was: False, now: True
```

The compiler only routed *code-bearing* patterns (`{ ... }`, `<?{ ... }>`,
`:my`/`:let`) through the defining-scope capture machinery added by
`b07ee6627`. A plain interpolating pattern like `/ $word /` compiled to a bare
constant, so a regex returned from a sub lost `$word` the moment its defining
frame was gone — and even a same-frame code-bearing regex read a stale
snapshot instead of the live binding after a later mutation.

The fix widened the compiler trigger (`regex_literal_closure_captures()`) to
cover any pattern with a resolvable interpolated name, and tracks which of
those names are mutated after the regex literal runs
(`CompiledCode::needs_cell_regex`, computed alongside the existing
`needs_cell_locals` free-variable analysis). `capture_regex_closure()` boxes
only the flagged names into a shared `ContainerRef` cell before reading them
— the same mechanism ordinary closures use — so later writes to the defining
frame flow through the same cell the stored regex holds, while unmutated
captures stay cheap by-value snapshots. `install_regex_closure_scope` was
already wrapped around `~~`/`.match`/`.subst`; `.split`/`.comb` were audited
and wrapped too (both can take a Regex-valued matcher argument). `s///` and
`TR///` needed no change — their pattern is always compiled inline with the
literal text baked into the executing frame, so they never carry an escaping
`Value::RegexCaptured`.

## Bug 2: the capture-isolation half

```raku
my $inner = rx/ (\d+) /;
my $outer = rx/ 'n=' <$inner> /;
"n=123" ~~ $outer;
say ($/[0] // 'undef');   # was: |123|, now: undef
```

A `<$var>` call — and its bare-`$var` / `<@var>` siblings — resolved the
stored regex's pattern text and spliced it into the outer pattern as a plain
`Group`, which is transparent to capture accumulation: the inner pattern's
positional *and* named captures numbered themselves straight into the
*outer* match. Raku gives a `<$var>`-family call its own discarded `Match`
object; no capture escapes.

The first attempt at a fix was a parse-time transform (`strip_captures_pattern()`,
modeled on the existing `strip_marks_pattern()` traversal used for `:m`) that
recursively degraded every `CaptureGroup` to a plain `Group` and cleared each
token's capture-related fields. It passed every local and roast check — but
the bundled-battery gate caught what those suites missed: `Cro::HTTP`'s
`http-request-serializer.rakutest` builds its multipart MIME boundary with
`Q/'boundary="' $<b>=[<-["]>+] '"'/` and then *reuses* `$<b>` later in the
same interpolated pattern as a backreference, to require the same boundary
string at the closing delimiter. Checked directly against `raku`, an
interpolated sub-pattern's OWN internal backreference to its OWN capture
keeps working even though the capture is invisible to the *outer* match — a
distinction plain capture erasure cannot express, since it destroys the
capture bookkeeping needed for the backreference in the first place, not just
its visibility to the caller.

The corrected fix adds a real match-time isolation boundary instead: a new
`RegexAtom::CaptureIsolatedGroup` variant matches its wrapped `RegexPattern`
exactly like `Group` (so internal captures resolve normally, including
backreferences within the same sub-pattern), but the two call sites that
merge a matched atom's captures into the caller's `RegexCaptures` — the
single-candidate and all-candidates matchers in `regex_match_capture.rs` /
`regex_match_atom.rs` — simply discard everything from a
`CaptureIsolatedGroup` match except its end position, instead of merging
`named`/`positional` the way they do for `Group`. It is constructed at the
`<$var>` tokenizer arm and in `array_var_alternation_atom` (the `<@var>`
form, via a `wrap_capture_isolated()` helper). Bare `$var`/`${name}`
interpolation of a Regex value emits the text `<$name>` instead of splicing
the pattern body directly, reusing the `<$var>` arm rather than duplicating
the logic; the bare-`@name` alternation form does the analogous `<@name>`
reroute when any element is a Regex value. String-only interpolations are
untouched in both cases. Every other `RegexAtom::Group`-handling site in the
regex engine (casefolding, LTM ranking, mark-stripping, capture counting,
quantified-name collection, ...) was audited by hand for how it should treat
the new variant — most already fall through a conservative default (treat it
like an opaque/subrule construct) that turns out to be exactly correct.

Two narrower corners were left as `// TODO`s for a future pass: the
`Junction` arm of `push_value_as_regex_pattern` (an `any(rx/(a)/, ...)`
interpolated into a pattern still leaks), and `@$var` (dereferencing a scalar
to an array, as opposed to a literal `@name`) has no `<@var>`-style tokenizer
form to reroute through.

`t/regex-stored-closure-scope.t` (18 subtests, every assertion verified
against real `raku`, including the Cro boundary-backreference shape) pins
both halves. The full `t/` suite (3168 files, 29462 assertions), the roast
files most adjacent to this area — `S05-interpolation/regex-in-variable.t`,
`S05-interpolation/lexicals.t`, `S05-capture/subrule.t`,
`S05-metasyntax/regex.t`, `S05-capture/caps.t`, and all 94 whitelisted
`S05-*` files as a broader spot-check — and `Cro::HTTP`'s
`http-request-serializer.rakutest` (17/17, run directly against the fetched
upstream suite) all stayed green.
