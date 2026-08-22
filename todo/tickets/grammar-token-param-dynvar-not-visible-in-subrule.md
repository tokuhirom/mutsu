# Grammar token-parameter dynamic variable not visible inside a called subrule

Found during the XML battery survey (`docs/batteries/xml.md`) while investigating why
the pure-Raku `XML` module (`raku-community-modules/XML`) fails to parse even the
simplest XML document on mutsu ("could not parse XML" from every test file that
calls `from-xml`).

## Root cause

`XML::Grammar` (`lib/XML/Grammar.rakumod` in the `XML` dist) declares its quoted-value
token with a **dynamic-variable parameter that carries a default value**:

```raku
token value($*STOPPER = '"') {
    \"
    [
    | \"
    | <char>+ \"
    ]
}
token char {
    <?{ $*STOPPER eq '"' }>
    <!["]> .
}
```

`value` sets `$*STOPPER` (a dynamic/contextual variable) as part of its own parameter
list, then calls the separately-declared subrule `char`, which reads `$*STOPPER` back
via a code assertion. This is a normal, documented Raku grammar idiom for
parameterizing a shared subrule from its caller (`raku-doc/doc/Language/regexes.rakudoc`
covers regex/token parameters; `variables.rakudoc` covers dynamic-scope `$*` lookup).

On mutsu, `$*STOPPER` reads back as `Nil` inside `char`, i.e. the dynamic variable set
by `value`'s own parameter binding does not propagate into the dynamic scope of a
subrule token it calls.

## Minimal repro

```raku
grammar G {
    token TOP { <value> }
    token value($*STOPPER = '"') {
        <char>
    }
    token char {
        { say "STOPPER is ", $*STOPPER.raku }
        .
    }
}
G.parse('x');
```

- `raku`: prints `STOPPER is "\""` (the default value is visible).
- `mutsu` (`target/debug/mutsu`): prints `STOPPER is Nil`.

A closer repro matching the actual failure shape (alternation + code assertion +
quantified subrule call, as in `XML::Grammar`):

```raku
grammar G {
    token TOP { <value> }
    token value($*STOPPER = '"') {
        \"
        [
        | \"
        | <char>+ \"
        ]
    }
    token char {
        <?{ $*STOPPER eq '"' }>
        <!["]> .
    }
}
my $m = G.parse('"hello"');
say $m ?? "matched: $m" !! "no match";
```

- `raku`: `matched: "hello"`.
- `mutsu`: `Use of Nil in string context` warning, then `no match` (the code assertion
  fails because `$*STOPPER` reads as `Nil`, so `Nil eq '"'` is false).

Isolated to confirm the token-parameter-default itself is fine when read from *within
the same token* (`token value($*STOPPER = '"') { ... }` reading `$*STOPPER` directly in
its own body works); the bug is specifically about visibility **across a subrule call**
made from that token's body.

## Why this matters beyond XML

This is a general grammar/regex-engine gap, not XML-specific: any grammar using the
"parameterize a token with a dynamic variable, then delegate to a shared subrule that
reads it back" idiom will hit the same silent-`Nil` failure. It fully blocks the `XML`
dist's own parser (`XML::Grammar`), which is otherwise a healthy, 0-dependency,
45-dependent, actively-referenced pure-Raku candidate — see `docs/batteries/xml.md` for
the full survey. 14 of 16 upstream test files fail this way (`could not parse XML` /
silently mis-parsed values); only `t/numeric-entities.rakutest` (pure string-function
tests, no grammar involved) passes.

## Affected files (starting point, not exhaustive)

- `src/vm/` grammar/regex execution — wherever token-parameter default bindings are
  installed into the dynamic scope (`$*`-lookup) versus the ordinary lexical scope.
  Grep for how `Grammar`/regex subrule calls thread the caller's frame relative to
  dynamic variables (`PSEUDO::DYNAMIC` / `$*` resolution in `runtime/regex.rs` /
  `runtime/regex_parse.rs`).

Not root-caused further within this survey's time budget — this ticket records the
minimal repro so the next session can dive straight into the regex/grammar dynamic-scope
code without re-deriving it.
