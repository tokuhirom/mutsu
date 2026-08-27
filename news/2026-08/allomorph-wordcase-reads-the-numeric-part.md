# `.wordcase` on an allomorph now reads its Str part and preserves the allomorph type

Follows on from the `.ord`/`.ords` allomorph fix (see
[news/2026-08/main-allomorph-arg-name-corrupts-later-intstr-new.md](main-allomorph-arg-name-corrupts-later-intstr-new.md)),
which found `.wordcase` on an allomorph reading the inner *number* instead of
its Str part, and left it for a separate decision because its correct return
*type* wasn't obvious:

```raku
my $a = IntStr.new(0, "zero one");
say $a.wordcase.raku;
# raku : IntStr.new(0, "Zero One")
# mutsu (before): "0"
# mutsu (after):  IntStr.new(0, "Zero One")
```

## The investigation: is the allomorph-preservation deliberate, or an artefact?

A sweep of every 0-arg `Cool` string method on `IntStr.new(99, "zero one")`
(rakudo 2026.06) gives this table:

| Method | Return type | Value |
|---|---|---|
| `uc`, `lc`, `tc`, `fc`, `tclc` | `Str` | case-transformed |
| `trim`, `trim-leading`, `trim-trailing` | `Str` | trimmed |
| `flip` | `Str` | reversed |
| `chars`, `ord` | `Int` | numeric result |
| `comb`, `ords` | `Seq` | list result |
| `chop`, `chomp` | `Str` | text result |
| **`wordcase`** | **same allomorph type** | **title-cased text, but numeric part reset** |

`wordcase` is the *only* outlier: every other member of the family returns a
plain `Str` (or the natural non-string type for `chars`/`ord`/`comb`/`ords`),
matching mutsu's existing (correct) behavior. Only `wordcase` hands back
another allomorph.

Digging further (`Cool.^find_method("wordcase").package` resolves to `Cool`
for `Str`, `Int`, and every allomorph type — there is exactly one candidate,
so the type-preserving behavior comes from inside that single method body, not
from a type-specific override):

- The reconstructed allomorph's numeric part is **not** the original number.
  `IntStr.new(99, "zero one").wordcase` is `IntStr.new(0, "Zero One")` —
  the `99` is gone, replaced by `0` unconditionally. Retrying with a numeric
  *string* (`IntStr.new(5, "123").wordcase`) still gives `IntStr.new(0,
  "123")`, so this isn't "renumify the wordcased text" either — it's always
  the type's zero value.
- `NumStr` resets to `0e0`, `ComplexStr` resets to `0+0i` — both clean,
  usable zeros, matching the `IntStr` pattern.
- `RatStr` is different: probing `RatStr.new(1/2, "a b").wordcase` shows
  `.^name` is still `RatStr` and `.Str` works fine ("A B"), but the
  reconstructed Rat's denominator is truly `0` — every numeric coercion of
  the result (`+$r`, `.Rat`, `.Int`, `.raku`) raises `Attempt to divide by
  zero when coercing/calling ...` in real rakudo. So rakudo's own
  `RatStr.wordcase` result is genuinely broken for numeric use, not just
  cosmetically "reset."
- The behavior is identical for the 0-arg and argument-taking (`:filter`,
  `:where`) forms — this isn't specific to one multi candidate.

Conclusion: allomorph-preservation is real and applies uniformly (not an
accident of a single call form), but the *numeric value* it produces carries
no information — it's always the type's zero, and for `RatStr` even that
"zero" is actually uninitialized and crashes on use. This reads as rakudo's
`Cool.wordcase` reconstructing the result via something like `self.new(0,
wordcased_str)` (supplying only a placeholder for the numeric slot), which
happens to construct cleanly for `Int`/`Num`/`Complex`'s trivial zero but not
for `Rat`'s two-part representation.

## What mutsu does

Rather than delegate `wordcase` to the shared plain-`Str` allomorph list (which
would fix the *value* but return the wrong *type*), mutsu now special-cases it:
the result preserves the allomorph type, with the numeric part reset to a
*safe* zero for that type (`Value::int(0)`, `Value::num(0.0)`,
`make_rat(0, 1)`, `Value::complex(0.0, 0.0)`). This matches rakudo's observable
behavior (type, string value) exactly for `IntStr`/`NumStr`/`ComplexStr`, and
for `RatStr` uses the sane `0/1` zero Rat instead of reproducing rakudo's own
crash-on-use bug — there is no reason to import an actual defect just for
bug-compatibility, especially since no roast test exercises this corner.

Implementation: a new shared helper,
`allomorph_wordcase_result()` in `src/value/types.rs`, builds the
zero-numeric + wordcased-Str mixin; it's called from both the 0-arg dispatch
(`src/builtins/methods_0arg/mod.rs`) and the argument-taking dispatch
(`dispatch_wordcase` in `src/runtime/methods_string.rs`), so `.wordcase` and
`.wordcase(:where(...), :filter(...))` behave consistently.

## Tests

`t/numeric-coercion-gaps.t` gained assertions (now 98, up from 79) covering:
value and return-type for `.wordcase` on `IntStr`/`RatStr`/`NumStr`/
`ComplexStr`, the argument-taking form, and three control methods (`.uc`,
`.trim`, `.flip`) that must keep returning a plain `Str`. All 98 pass under
both `raku` and mutsu.
