# Fix a false positive in the `<$var>` regex-interpolation security check

`<$var>` regex interpolation compiles the string as regex source, and
mutsu runs `contains_dangerous_regex_code` on it first to reject actual
code-injection constructs (`X::SecurityPolicy`, matching Rakudo's
restriction on interpolated regex source). A legitimate pattern like
Cro's multipart boundary matcher was incorrectly rejected:

```
my $p = Q/'boundary="' $<b>=[<-["]>+] '"'/;
my $s = 'Content-type: multipart/form-data; boundary="abc123"';
say ?($s ~~ /<$p>/);   # mutsu (before): dies "Prohibited regex interpolation"   raku: True
```

## Root cause

The "double-quoted strings with interpolation" check split the pattern
on every `"` character and treated odd-indexed chunks as "inside double
quotes", flagging a `$`/`@`/`%`/`&` found there. This parity count has no
concept of a single-quoted literal or a character class: the Cro pattern
contains `"` both inside a single-quoted literal (`'boundary="'`) and
inside a negated character class (`<-["]>`, matching anything but `"`) —
neither is a real double-quote region, but the naive split still counted
them, throwing off the parity and misclassifying the legitimate
`$<b>=[...]` named-capture-alias syntax as "inside quotes".

**A word on the rest of the heuristic.** An earlier diagnosis of this
ticket proposed dropping several of the OTHER checks
(`<$`/`<@` nested interpolation, `$(`/`@(`, the double-quote heuristic
entirely, and `<\w+(...)>` subrule-with-args) as overly broad. Verifying
each against `raku` directly first (per the project's investigation
protocol) showed every one of those claims was wrong — real Rakudo
rejects ALL of them with the exact same `X::SecurityPolicy`, including
nested `<$x>` interpolation, bare `$(...)`  with no quotes at all, and
`<alpha(3)>`-style subrule calls with arguments. Only the double-quote
parity check's blindness to quotes-within-quotes and char classes was a
genuine bug; the fix here is narrower than originally proposed and
touches only that one block.

## Fix

Rewrote the double-quote-chunk check as a small state machine tracking
single-quote state and `[...]` character-class depth, so a `"` inside
either is never treated as opening/closing a double-quote region. Every
other check (`{`/`}`, `<$`/`<@`, `$(`/`@(`, `::(`, the subrule-with-args
regex, `:my `/`:our `, `:(`) is untouched.

## Verification

- The Cro repro now matches (`True`), same as raku.
- `roast/S05-interpolation/regex-in-variable.t` (66 subtests, including
  every `X::SecurityPolicy` rejection case — unbalanced braces, dynamic
  lookups, genuine double-quote sigil interpolation, `<{ }>` code
  blocks) still passes in full: nothing was loosened.
- `t/http-request-serializer.rakutest` (vendored Cro::HTTP suite) now
  passes `1..17` in full — this was the last of its two blockers (the
  first, a `.map`/`when` succeed-signal bug, was fixed separately).
- New pin: `t/regex-interp-capture-alias.t`.
- The `S05-regex`/`S03-smartmatch/regex-hash`/`S15-nfg/regex` roast
  sweep and the full `make test` suite pass with no regressions.
