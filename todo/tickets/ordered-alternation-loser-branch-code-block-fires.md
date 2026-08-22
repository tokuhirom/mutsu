# A `||` alternation runs the losing branch's code block even when the first branch wins

## Symptom

In `[ A || B { code } ]` the second branch's `{ ... }` block executes even
though `A` matched and `B` was never supposed to be tried:

```raku
my @fired;
grammar P { token TOP { 'a' [ 'b' || . { @fired.push('P') } ] } }
say P.parse('ab') ?? "matched" !! "no", " fired=", @fired.raku;
```

- raku: `matched fired=[]`
- mutsu: `matched fired=["P"]`

The match itself is correct — only the side effect is spurious. It reproduces
with the first branch spelled as a literal, a subrule, or a proto subrule, and
whether the alternation sits directly in `TOP` or in a nested token. (It does
NOT reproduce when the alternation is inside a token reached through a subrule
*and* has no code block, which is why it hid for so long: the failing shape
needs both the code block and the branch that loses.)

## Why it matters

A `{ ... }` block in a losing branch is the standard Raku idiom for "everything
else here is an error", so this turns a *successful* parse into a thrown
exception:

```raku
token string-basic-char:escape-sequence {
    \\ [ <escape> || . { die(X::...::EscapeSequence.new(:esc(~$/))) } ]
}
```

That is verbatim `Config::TOML::Parser::Grammar`, and it is why parsing any TOML
document containing a `\n`/`\"`/`\\` escape dies with "Sorry, found bad string
escape sequence" under mutsu even though `<escape>` matched fine
(`grammar/04-document.rakutest`, `grammar-actions/04-document.rakutest`,
`special-cases/06-multiline-string-ws-remover-leading-ws.rakutest`; see
`docs/batteries/toml.md`).

## Where to look

ADR-0009 and `declarative_prefix_match_len`
(`src/runtime/regex/regex_resolve.rs`) already establish that LTM prefix
measurement must never *execute* a code atom, and `LTM_DECLARATIVE_MODE` exists
to enforce that. Two candidate explanations, in order of likelihood:

1. The `||` (ordered) alternation is being measured or walked like `|` (LTM),
   so the second branch is entered for measurement with code execution still
   enabled — the flag is a thread-local, so check that it is actually set on
   this path.
2. The alternation walker tries branches for *capture* purposes after a winner
   is chosen.

`t/` has no pin for "a losing `||` branch must not run its code block"; add one
with the repro above alongside the fix.

## Repro (three shapes, all wrong under mutsu)

```raku
my @fired;
grammar P { token TOP { 'a' [ 'b' || . { @fired.push('P') } ] } }
grammar Q { token TOP { 'a' [ <b> || . { @fired.push('Q') } ] }; token b { 'b' } }
grammar R { token TOP { <inner> }; token inner { 'a' [ 'b' || . { @fired.push('R') } ] } }
for P, Q, R -> $g { @fired = (); $g.parse('ab'); say $g.^name, " fired=", @fired.raku }
```
