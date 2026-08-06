# `duckmap` itemizes the sublist it descended into

```raku
say (1, (2, 3)).duckmap(-> Int $x { $x * 10 }).raku;   # (10, $(20, 30)) — mutsu printed (10, (20, 30))
say (1, [2, 3]).duckmap(-> Int $x { $x * 10 }).raku;   # (10, $[20, 30])
say (1, (2, 3).Seq).duckmap(-> Int $x { $x * 10 }).raku; # (10, $(20, 30)) — was (10, (20, 30).Seq)
say (1, %(a => 2)).duckmap(-> Int $x { $x * 10 }).raku;  # (10, ${:a(20)})
```

When the block rejects an element, `duckmap` descends into it — and rakudo
itemizes what comes back, so the sublist is one element of the result
rather than something that can flatten. `duckmap_element`'s descend arms
(`src/runtime/builtins_collection_deepmap.rs`) returned the plain
container.

A raku baseline sweep confirmed the rule is exactly `deepmap`'s
`itemize_result` distinction: a **List/Seq/Hash parent itemizes** its
element descends, a **real Array parent does not**
(`[1, [2,3]].duckmap(...)` stays `[10, [20, 30]]`), and itemization
applies at every nesting level. Two Seq corners came along: a Seq
*descend* comes back as an itemized **List** (not a Seq), and duckmap on
a Seq *invocant* returns a List (`(...).Seq.duckmap(...).WHAT` is
`List`). A Hash descend itemizes by wrapping in a `Value::scalar`
(`${:a(20)}`).

`duckmap_element` now carries an `itemize` flag threaded from the parent
container's kind, mirroring `deepmap_iterate_inner`. The
`t/deepmap-on-a-range.t` duckmap assertion that compared a Range descend
against the equivalent List (because neither itemized) is tightened to
raku's literal output, as that test's comment promised.

Pinned by `t/duckmap-itemizes-descend.t` (12 cases, verified against
raku).
