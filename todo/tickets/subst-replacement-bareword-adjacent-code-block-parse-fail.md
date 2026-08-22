# `s///` replacement text with a literal word immediately followed by an embedded `{...}` code block is a hard parse error

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/operators.rakudoc:214`).

Related to, but distinct from, the already-filed
[subst-replacement-code-block-not-evaluated.md](subst-replacement-code-block-not-evaluated.md)
(that ticket's repro parses fine and mostly evaluates its embedded code blocks correctly — its
bug is a stray backslash on `\:`). This finding is a **compile-time parse failure**, not a
runtime interpolation bug.

## Root cause hypothesis

A substitution replacement string can mix literal text directly adjacent to an embedded `{...}`
code block, with no separator between them — the literal text ends and the code block begins
right where `{` appears:

```raku
$str ~~ s :g :i/<[ML]> (\S+)/d{lc $0}/;
```

Here the replacement text is the literal letter `d` followed immediately by the code block
`{lc $0}` (no space, no explicit concatenation operator). mutsu fails to compile this at all:

```
Confused. expected statement: expected expression statement or expression after additive operator or '.' or digits or generic radix literal or ...
------>$str ~~ s :g :i/<[ML]> (\S+)/d{lc $0}/;
                              ^
binary operators require a right-hand expression.
```

The error text ("binary operators require a right-hand expression") suggests the
replacement-string parser is treating the bareword-like `d` as the start of an expression (and
then choking when `{` follows it with no operator in between), rather than treating `d` as
**literal replacement text** and `{lc $0}` as a separately-recognized embedded interpolation
block — the same way a plain `"literal text{block}"` interpolated string already handles a
bareword immediately followed by `{`.

## Minimal repro

```raku
my $str = 'foo muCKed into the lEn';
$str ~~ s:2nd/o/x/;
$str ~~ s :g :i/<[ML]> (\S+)/d{lc $0}/;
say $str;
```

- `raku`: `fox ducked into the den`
- `mutsu` (`target/debug/mutsu`): fails to compile (`===SORRY!===` parse error, see above)

## Affected files (starting point)

- `s///` replacement-string parsing/lexing, likely near where interpolated strings and
  embedded code blocks are lexed for substitution replacement text — `src/parser/` (quote/regex
  slang replacement handling) and possibly `src/vm/vm_string_regex_ops.rs`.
