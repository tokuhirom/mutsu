# `$<a>=<$re>` keeps the called regex's nested captures

A scalar sigil alias on a call of a Regex-valued variable named only the matched
span and dropped every capture inside the regex:

```raku
my $r = /<digit>+/;
say ("12" ~~ /$<rx>=<$r>/)<rx><digit>.elems;   # rakudo 2, mutsu 1
```

Rakudo's `subrule_alias` renames a subrule call under a sigil alias, so
`$<rx>=<$r>` is `<rx=$r>`: `rx` holds the called regex's own Match, with its
nested captures, and nothing leaks into the outer match. mutsu already did that
for the angle spelling (`<rx=$r>`, `news/2026-10/css-specification-interpolated-alias.md`),
which resolves the variable at match time as an anonymous subrule. The sigil
spelling went through the parse-time `<$var>` arm instead, which wraps the parsed
regex in a `CaptureIsolatedGroup` — correct for an unaliased call, whose Match is
discarded, but it left the alias nothing to nest.

## Fix

The tokenizer's `<`-arm now spells a pending scalar sigil alias on a
Regex-valued `<$var>` call as `<alias=$var>` and drops the pending alias
(`Interpreter::sigil_aliased_regex_call`, `src/runtime/regex_parse_core.rs`), so
the existing match-time path serves it: nested captures, the per-repetition List
of a quantified call (`$<rx>=<$r>+`), a repeated alias listing each call, the
defining-scope install of a closure-valued regex, and both matcher engines.
The bare `$<rx>=$r` needs nothing of its own: the scalar-interpolation pass
already reroutes a Regex value through `<$r>`.

Left on the old path, deliberately:

- an alias on a group (`$<rx>=[<$r>]`, `$<rx>=(<$r>)`) — rakudo keeps those a
  plain span too, and the tests pin it;
- a numbered alias (`$0=<$r>`) and an array alias (`@<rx>=<$r>`) — the compiled
  matcher only files an alias that sits on the token, so the array form's forced
  List would be lost;
- a variable holding a `Str` pattern — the match-time lookup serves Regex
  values only.

These three are filed together as [#10673](https://github.com/tokuhirom/mutsu/issues/10673).

## Tests

`t/regex/regex-sigil-alias-regex-var-call.t` (23 rows, all measured on rakudo
2026.07 and passing under 2026.09 too): the ticket's repro, span and extent of
the alias, no leak into the outer match, the bare form, the angle twin, an alias
inside a longer pattern, two different aliases, a repeated alias, a quantified
call, and the group-wrapped forms that must stay spans. The file also passes with
`MUTSU_RX_VM=0`, so both matcher engines agree.
