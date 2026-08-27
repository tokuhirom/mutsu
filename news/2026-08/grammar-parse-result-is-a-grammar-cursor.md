# `G.parse(...)` answers a `G` cursor, not a plain `Match`

In raku a `Grammar` IS a `Match` subclass (`Grammar.^mro` is
`(Grammar Match Capture Cool Any Mu)`) and every cursor a parse mints is of the
*invoked* grammar's own type. mutsu handed back a plain `Match` for all of them:

```raku
grammar G { token TOP { <a> }; token a { \w+ } }
say G.parse("hello").WHAT;    # raku: (G)      mutsu (before): (Match)
say G.parse("hello").^name;   # raku: G        mutsu (before): Match
```

A grammar that declared attributes was worse: raku's `G.parse(...).invalid`
answers the (undefined) attribute, mutsu died with
`No such method 'invalid' for invocant of type 'Match'`.

## The measured contract

Everything below was measured against real raku before any code was written, and
is now pinned by `t/grammar-parse-result-cursor-type.t` (40 assertions, which
also pass verbatim under `raku`):

- `G.parse(...)` / `.subparse(...)` / an instance's `.parse` all answer a `G`.
- **Nested cursors are cursors too**: `$m<a>` and `$m[0]` are `G`, not `Match`.
- The **invoked** grammar wins over the token's owner: for `grammar H is G`,
  `H.parse(...)<a>` is an `H` even though `token a` came from `G`.
- A cursor is still a `Match`: `~~ Match`, `~~ Capture`, `~~ Cool`,
  `.isa(Match)`, `isa-ok $m, Match` all hold, and the whole `Match` interface
  (`.from`/`.to`/`.orig`/`.chars`/`.Str`/`.list`/`.hash`/`.gist`/`.made`) answers.
- A **failed** `.parse` is `Nil` (unchanged).
- A **plain regex match stays a bare `Match`** — including one taken inside a
  grammar's own method body. That is the negative control.
- `.raku` of a cursor still renders `Match.new(...)`, and raku holds
  `EVAL($cursor.raku) eqv $cursor` across the class difference.

## Mechanism: a per-parse cursor class on the shared `MatchTarget`

The ticket named two candidate mechanisms — a per-Match-node class symbol, or
giving grammar classes an MRO that includes `Match` and returning a grammar
*instance* that answers every `Match` method. The second would have thrown away
the ADR-0016 P5 lazy `Match` representation for every grammar parse, so the
first was taken, with one refinement that made it much smaller than expected:

**the cursor class is a per-parse-run property, and `MatchTarget` already IS the
per-parse-run object.** One target is created at the engine entry and shared by
every node of the resulting tree, so the class was added there rather than to
`MatchNode`, behind a shared `Arc<AtomicU32>`. `Grammar.parse` stamps it on the
finished result (`MatchTarget::set_cursor_class`), and because the cell is
shared, every already-cloned child target sees it — the whole cursor tree retags
at once and **no class has to be threaded through the regex engine**. Nested and
inherited-token cursors get the invoked grammar's class for free, which is
exactly raku's rule. A plain regex match never stamps, so it stays `Match`.

Three supporting pieces:

1. **Real `Grammar` → `Match` inheritance.** mutsu already reported
   `G.^mro` as `(G Grammar Match Capture Cool Any Mu)`, but only as a
   display-level patch in `classhow_mro_names`; `G.new ~~ Match` was `False`
   because smartmatch/isa go through the registry MRO, and `Grammar` had no row
   in the ADR-0051 builtin type catalog. Adding that row makes the inheritance
   real, which is what carries `~~ Match` / `~~ Capture` / `~~ Cool` for cursors.

2. **The "is a Match" test is the repr, not the class name.** Roughly 40 sites
   across `value/`, `builtins/`, `vm/` and `runtime/` guarded behaviour with an
   inline `class_name == "Match"` — `.gist`, `.raku`, `.Str`, subscripting,
   smartmatch, `for` iteration, `Capture`/`Hash` coercion, failed-match
   falsiness. All were migrated to the ADR-0016 P5 seam predicate
   `Value::is_match_instance()`, which answers from the `ValueRepr::Match` tag
   with no registry lookup. Eager rebuilds (`match_with_attrs*`, the action
   walk's write-backs) are plain `Instance`s where the class name is no longer
   the signal, so `MatchNode::materialize_map` writes a `__grammar_cursor__`
   marker into the attribute map *once*; every derivative inherits it by copying
   the map, so no construction site has to know about cursors.

3. **Dispatch owner.** The `"Match"`-hardcoded user-override / native-method
   lookups (`should_bypass_native_fastpath`, `try_native_method_raw`,
   `dispatch_mro`, hyper method calls, `~` coercion) now ask
   `Value::match_dispatch_class()`. Those lookups already MRO-walk, so `Match`'s
   own rows still apply — but a `method` the grammar body declares now wins over
   the native row, the way raku's MRO makes it.

## Anonymous grammars: a cursor is a Match by SHAPE

A grammar declared as a *statement* reaches `Match` through its `Grammar` parent
and the catalog row. An **anonymous** grammar (`my grammar { … }`, the shape
`t/grammar-capture-markers.t` exercises) does not: it is registered as a bare
package with no parents at all, so before this change its cursors related to
`Match` only because they literally *were* class `Match`. Three sites therefore
assert the invariant from the value's shape rather than its registration —
`Value::isa_nominal_hierarchy` gained a `"Match"` arm, and the `~~`
(`type_matches_value`) and `.isa` (`methods_instance_ops`) fallbacks consult
`isa_check` for a Match receiver before answering `False`. `dispatch_mro`
likewise splices the `Match` chain onto a cursor whose grammar chain does not
reach it. (That anonymous grammars relate to *no* type — `$g ~~ Grammar` is also
`False` — is a separate pre-existing gap in how the anon-grammar expression
registers its package; it is untouched here.)

## Two real bugs this exposed

- **`regex_match_atom.rs` misread a returned cursor as `self`.** A grammar
  method assertion that returns a value was treated as the idiomatic
  `{ …; self }` zero-width success whenever the value's class equalled the
  grammar package. That test was only ever safe because a parse cursor reported
  `Match`; once cursors carry the grammar's class, a method returning a real
  sub-match had its extent swallowed. It now also requires the value NOT to be a
  Match — a Match belongs to the extent branch. This is what broke the bundled
  YAMLish battery (`load-yaml` returned `Any`) and is a genuine pre-existing
  fragility, not a cost of this change.

- **`eqv` compared Match objects by class.** raku holds
  `EVAL($cursor.raku) eqv $cursor` even though the round trip is a plain
  `Match`; mutsu's generic Instance arm compared class names and attribute maps.
  Two Matches now compare by content (from/to/orig/list/named), which is what
  `roast/S05-match/raku.t` asserts.

## Known remaining divergence

Reading an unset grammar attribute off a cursor answers `Nil` where raku answers
the type object (`Any`). Both are undefined, and the dispatch error is gone; the
`Nil`-vs-`Any` gap is mutsu's general behaviour for an uninitialised attribute,
not specific to cursors.
