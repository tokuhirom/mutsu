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

The fix is a parse-time transform rather than a new engine-wide `RegexAtom`
variant: `strip_captures_pattern()` (`src/runtime/regex/regex_helpers.rs`,
modeled on the existing `strip_marks_pattern()` traversal used for `:m`)
recursively degrades every `CaptureGroup` to a plain `Group` and clears
`named_capture`/`secondary_named_capture`/`hash_capture`/`force_list_capture`
on every token, everywhere a sub-pattern can nest (groups, alternations,
conjunctions, lookarounds, goal-matches, separators). It is applied at the
`<$var>` tokenizer arm and in `array_var_alternation_atom` (the `<@var>`
form). Bare `$var`/`${name}` interpolation of a Regex value now emits the
text `<$name>` instead of splicing the pattern body directly, so it reuses
the same `<$var>` arm (and its capture stripping) rather than duplicating the
logic; the bare-`@name` alternation form does the analogous `<@name>`
reroute when any element is a Regex value. String-only interpolations are
untouched in both cases.

Two edges are accepted rather than chased down: an inner pattern's *own*
backreference to its *own* capture (`rx/(\w)$0/` invoked via `<$var>`) stops
working once the capture group is degraded to a non-capturing one — fixing
that needs match-time isolation via a dedicated `RegexAtom` variant, not this
parse-time transform. And two narrower corners were left as `// TODO`s for a
future pass: the `Junction` arm of `push_value_as_regex_pattern` (an
`any(rx/(a)/, ...)` interpolated into a pattern still leaks), and `@$var`
(dereferencing a scalar to an array, as opposed to a literal `@name`) has no
`<@var>`-style tokenizer form to reroute through.

`t/regex-stored-closure-scope.t` (18 subtests, every assertion verified
against real `raku`) pins both halves. The full `t/` suite (3168 files, 29462
assertions) and the roast files most adjacent to this area —
`S05-interpolation/regex-in-variable.t`, `S05-interpolation/lexicals.t`,
`S05-capture/subrule.t`, `S05-metasyntax/regex.t`, `S05-capture/caps.t`, and
all 94 whitelisted `S05-*` files as a broader spot-check — stayed green.
