# `.parse(..., :args(...))` binds a start rule's dynamic parameters

```raku
grammar demonstrate-arguments-dynamic {
   rule TOP ($*word, $*extra) { <phrase-stem><added-words> }
   rule phrase-stem { "I like" }
   rule added-words { $*word $*extra }
}
say demonstrate-arguments-dynamic.parse("I like everything else",
  :args(("everything", "else")));
```

returned `Nil` on mutsu where raku produces the match. `:args` with *ordinary*
parameters already worked, so the failure was specific to `$*` parameters.

## Root cause, part one — shared with the sibling ticket

The main cause is the same one written up in
[grammar-token-param-dynvar-not-visible-in-subrule](grammar-token-param-dynvar-not-visible-in-subrule.md):
a `$*`-twigil parameter was bound only inside the scratch interpreter that turns a
rule body into a pattern string, and never reached the dynamic scope, so
`added-words` interpolated nothing. On top of that, `dispatch_package_parse` bound
the `:args` values *after* it had already asked the start rule for its pattern, so
even the start rule's own pattern was built with the parameters unset. The
parameters are now established before the start rule's pattern is resolved and torn
down when the parse ends.

## Root cause, part two — a `rule` swallowed the `<.ws>` before a twigilled variable

That alone was not enough: `rule TOP ($*w) { "x" $*w }` still failed while
`rule TOP ($w) { "x" $w }` worked. `rule`'s sigspace pass rewrites source
whitespace between atoms into an explicit `<.ws>`, but it suppresses the insertion
before a `$` on the theory that the `$` is the end-of-string anchor. It made an
exception for a `$` that begins a term — `$var`, `$0=…`, `$<name>=…`, `${…}` — by
peeking at the *next* character, and that peek accepted only alphanumerics, `_`,
`<` and `{`. A twigil (`*`, `?`, `^`, `.`) failed the test, so
`"x" <.ws> $*w <.ws>` came out as `"x" $*w <.ws>` and the pattern could never match
the space in the input. The peek now accepts twigils.

## A third bug found while pinning it

Writing the regression test surfaced an unrelated, pre-existing corruption:
`inject_separator_ws` — the pass that lets a `rule`'s `%` separator quantifier
absorb surrounding whitespace — did not skip `{ … }` code blocks, so a `%hash`
inside a block in a `rule` was mangled into `%[ <.ws>? h <.ws>? ]ash`. Fixed by
tracking brace depth there, as the sibling whitespace pass already did.

Verified against raku: the `:args` list form, the `\(...)` Capture form, ordinary
non-dynamic parameters, and dynamic parameters reaching a sibling subrule.

Pin: `t/grammar-dynvar-failgoal-ws.t`.
