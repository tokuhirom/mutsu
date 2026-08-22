# User-defined `circumfix:<...>` with a non-ASCII (Unicode-letter) delimiter fails to parse at the call site

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/optut.rakudoc:12`).

## Root cause hypothesis

Defining `sub circumfix:<α ω>( $a ) { ... }` itself parses fine. Calling it
(`α 5 ω;`) fails to parse: mutsu's tokenizer/parser does not recognize a bare
Unicode-letter identifier token (`α`) as a valid opening delimiter for a
previously-declared custom circumfix operator invocation. The equivalent ASCII forms —
single-token (`sub circumfix:<[[ ]]>`) and space-separated multi-word ASCII identifiers
(`sub circumfix:<FOO BAR>`, called as `FOO 5 BAR`) — both already work correctly, so
the gap is specifically in recognizing non-ASCII/Unicode-letter tokens as custom
circumfix-operator delimiters at the call site, not in circumfix-operator support in
general.

## Minimal repro

```raku
sub circumfix:<α ω>( $a ) {
    say $a * 2;
}
α 5 ω;
```

- `raku`: `10`
- `mutsu` (`target/debug/mutsu`):
  ```
  ===SORRY!=== Error while compiling ...
  Confused. expected statement: expected expression statement or expression after infix operator or '.' or digits or generic radix literal or ...
  ------>α 5 ω;
         ^
  ```

Confirmed the delimiter's Unicode-ness (not multi-char-ness) is the trigger:
`sub circumfix:<αX Xω>(...)` called as `αX 5 Xω;` fails the same way, while
`sub circumfix:<FOO BAR>(...)` called as `FOO 5 BAR;` works.

## Affected files (starting point)

The parser's custom-operator lookup for circumfix invocations (grep for where declared
`circumfix:<...>` operators are matched against upcoming tokens) — likely in
`src/parser/`, needs to accept a leading Unicode-letter/identifier token the same way
it already accepts an ASCII bareword token as a registered circumfix opener.
