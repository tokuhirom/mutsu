# A subrule call with a block-literal argument (`<name: { ... }>`) fails entirely

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1966).

## Repro

```
my regex demo ($param) {
    foo
    { say $param }
    bar
}
'foobar' ~~ / <demo: { key => <v a l> }> /
```

- raku: prints `{key => (v a l)}` (the subrule `demo` is called with a hash-literal-with-quote-
  words argument, and its embedded `{ say $param }` prints the received parameter)
- mutsu: `Nil` — the whole match fails, meaning either the subrule-with-argument call syntax
  isn't parsed as intended, or the argument expression itself (a block containing a Pair whose
  value is a `<...>` word-quote list) isn't evaluated/passed correctly.

## Root cause guess

Parametrized subrule calls (`<name: ARGS>`) with a simple argument (e.g. a variable or literal)
likely already work elsewhere in the codebase; this specific combination — a *block-literal*
argument containing a Pair whose value is itself a `<...>` word-quote — may be exposing either a
parser ambiguity (is `{ ... }` here a hash literal, a code block, or a Match closure?) or simply
an unimplemented argument-expression shape for subrule calls.

## Affected files (starting point)

- `src/parser/` — subrule-call argument parsing (`<name: ARGS>`)
- `src/runtime/regex.rs` — subrule invocation with arguments

## Suggested next step

Narrow further: does `<demo: {"literal"}>` (a plain string, no Pair/word-quote) already work?
Does `<demo: (key => "v")>` (parenthesized, not a bare block) work? Bisecting which piece of the
argument syntax breaks will scope the actual parser/runtime fix.
