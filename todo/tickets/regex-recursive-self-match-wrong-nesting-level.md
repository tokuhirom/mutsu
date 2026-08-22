# `<~~>` recursive self-match returns the wrong (inner, not outer) nesting level

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
2935).

## Repro

```
my $paren = rx/ '(' <-[()]>* ')' || '('[ <-[()]>* <~~> <-[()]>* ]* ')' /;
say '(1 + (2 x 3)) = 7' ~~ $paren;
say '((5 + 2) x 6) = 42 (the answer)' ~~ $paren;
```

- raku: `｢(1 + (2 x 3))｣` then `｢((5 + 2) x 6)｣` — the outermost balanced-parenthesis span
- mutsu: `｢(2 x 3)｣` then `｢(5 + 2)｣` — an *inner* balanced-parenthesis span, one recursion level
  too deep

The simpler cases (no nesting, or a single level of nesting matched from the top) already work
correctly (`(1 + 1)` case matches in both).

## Root cause guess

`<~~>` (recursive self-reference to the enclosing regex/rule, used here to match balanced nested
parens) likely returns the *innermost* successful recursive match instead of letting the
*outermost* invocation's match span win — i.e., the recursion's result propagation/backtracking
returns control to the wrong stack frame, or the outer alternation branch re-matches from the
inner recursive call's position instead of its own start.

## Affected files (starting point)

- `src/runtime/regex.rs` — `<~~>` recursive self-match implementation

## Suggested next step

Test with exactly one level of nesting deeper than the working case to see whether the "off by
one recursion level" is exact (matching a specific inner span each time) or grows worse with
deeper nesting — that will show whether the whole match-span stack is shifted by a constant
offset or whether something more structural is wrong with how recursion results are threaded back
to the outer match.
