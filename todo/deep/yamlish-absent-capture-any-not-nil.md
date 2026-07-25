# In the YAMLish grammar, an unmatched named capture reads as `Any`, not `Nil`

Blocker #3 (continued) for the `YAMLish` battery. With the grammar-parse /
`nextwith` fix (PR #5447) landed, `use YAMLish; load-yaml("42")` now *parses*
(the document `Grammar` matches), but `concretize` dies:

```
Failure.new("Couldn't parse YAML: Type check failed in assignment to $!root;
             expected YAMLish::Element but got Package")
```

## Root cause (isolated)

The `plain` action (`method plain($/)`, lib/YAMLish.rakumod:594) does:

```raku
my Tag $tag    = $<properties><tag>.ast;
my Str $anchor = $<properties><anchor>.ast;
```

For a bare scalar like `"42"` the `[ <properties> <.space>+ ]?` group does NOT
match, so `$<properties>` is an unmatched named capture. Under **raku**
`$<properties>` is `Nil`, and `Nil<tag>.ast` → `Nil` (Nil absorbs the method
call). Under **mutsu** `$<properties>` is **`Any`**, and `Any<tag>` → `Any`,
`Any.ast` → **throws** `X::Method::NotFound: No such method 'ast' for invocant of
type 'Any'`. (`Any.ast` throwing is itself correct — raku throws too — so the fix
is NOT to make `Any.ast` return Nil; the fix is that the absent capture must be
`Nil`.)

The `plain` action's exception is swallowed by the grammar action-dispatch
(best-effort), leaving `$<plain>.ast` undefined; the `simple-document` action
then reads `$/.values.[0].ast` = Nil and assigns it to `Document`'s
`has Element:D $.root is required`, where Nil is substituted by the `Element`
type object (a `Package`) → the type-check failure above.

## Reproduction (reliable)

Instrument a copy of the module (`tmp/yamllib/YAMLish.rakumod`) — add to the top
of `method plain($/)`:

```raku
note "props={$<properties>.^name}";
```

Then:

```sh
mutsu -I tmp/yamllib -I modules/MIME-Base64/lib -e 'use YAMLish; load-yaml("42")'
# mutsu:  props=Any   (twice)   <- BUG
# raku:   props=Nil
```

Both the `load-yaml` override path AND a direct
`YAMLish::Grammar.parse("42", :actions(YAMLish::Grammar::Actions))` reproduce it,
so it is NOT related to the `nextwith` override.

## Why it is hard / not yet minimally reproduced

Every hand-built minimal grammar with the same *shape* returns `Nil` correctly
(`tmp/yc1.raku`..`yc3.raku`, `tmp/yv1.raku`..`yv6.raku`): a `regex plain(Str
$indent)` with `[ <properties> <.space>+ ]?`, the `$<value>=[...]` captures, the
`| <block> | <root-block> | <inline> | <block-string> || <plain('')>`
alternation, and the real `properties { <anchor> ... | <tag> ... }` token — all
give `props=Nil`. So the trigger is some *additional* interaction in the full
grammar (~60 tokens, a nested `class Actions`) that has not been isolated.

Two anomalies seen only in the full grammar, likely related:

1. **The `plain` action runs TWICE** for a single parse (the minimal repros run
   it once). mutsu appears to dispatch grammar actions both at reduce time and
   post-parse; one of the two dispatches may carry a partial/stale match object
   in which `$<properties>` is `Any`.
2. Adding `note`/`CATCH` statements *between* the action's statements changed
   whether the throw surfaced — a heisenbug smell pointing at action-body
   compilation or the reduce/dispatch interleaving.

## Related finding: assertion subrules leak a spurious capture

While isolating the above, a separate, cleanly-minimal bug surfaced: a
zero-width assertion that calls a *user subrule* — `<!break>` / `<?break>` —
leaks a spurious named capture (`!break` / `?break`) onto the enclosing match:

```raku
grammar G { token TOP { <p> }  regex p { $<v>=[ \d [ <!b> ]* ] }  token b { X } }
say G.parse("4")<p>.hash.keys.raku;
# raku:  ("v",)
# mutsu: ("!b", "v")      <- spurious "!b"
```

Both `<!b>` and `<?b>` leak (`!b` / `?b`); it happens inside and outside a
`$<...>=[ ]` group. Root: `regex_parse_core.rs:2144` lowers a non-builtin
`<!name>` to `RegexAtom::Named("!name")`, and `parse_named_regex_lookup_spec`
(`regex/regex_resolve.rs:397`) handles the `.`/`=`/`&` prefixes but NOT the
`!`/`?` assertion prefixes, so the match stores a capture under the `!`/`?`-name.
Fix: an assertion-prefixed subrule (`!`/`?`) must be non-capturing (set the
spec's `silent`, without disturbing the negation logic that reads the `!`).
This is NOT the cause of the `props=Any` blocker (a grammar with the spurious
`!break` key still returns `props=Nil` in isolation — see `tmp/yv7.raku`), but it
is a real capture-set bug worth its own small PR.

## Next step

Bisect the FULL grammar (start from `tmp/ygbase.raku` = lines 1-783 of the module
with a `Grammar.parse("42", :actions(...))` harness, which reproduces
`props=Any`), deleting tokens until `$<properties>` flips back to `Nil`. Then fix
whichever mechanism reifies an unmatched named capture as `Any` instead of `Nil`
(likely in the grammar reduce / `invoke_grammar_actions` path, or the
double-dispatch). Confirm with `raku -e 'say (so "x" ~~ /x [<y=.alpha>]?/); say
$<y>.^name'` semantics: an unmatched named capture is `Nil`.
