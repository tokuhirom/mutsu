# In a `rule`, whitespace before `|` is significant

Under a `rule`'s sigspace, whitespace after an atom is a `<.ws>`. That is also
true when the next thing is a `|` or `||`: `rule e { a | b }` needs
whitespace after `a`, and `rule e { a| b }` does not. Only the whitespace
right after the pipe is layout.

mutsu's `inject_implicit_rule_ws` (`src/parser/stmt/class/token_body.rs`)
suppressed the `<.ws>` before a pipe as well. As a result, a branch that ended
in an optional atom could not step past the space that followed it.
`rule element { <atom> <ebnfSuffix>? | ... }`, repeated by `<element>*`, failed
on `r : 'a'? 'b' ;`. That made every ANTLR4::Grammar corpus file that uses
`?` or `*` on a non-final element parse as `False`. With the fix,
`ANTLR4::Grammar`'s `t/01-parse.t` passes and `t/04-use-parser.t` runs to the
end.

The corpus tests still time out. The cause is exponential LTM measurement
through the recursive `ACTION` token, now tracked in #9579.

Test: `t/grammar/rule-sigspace-before-alternation.t`. Refs #9491.
