# INIT and CHECK in a `token`/`rule`/`regex` body run once, at their own phase

```raku
my @log;
grammar G { token TOP { a { INIT @log.push('G') } b } }
say @log.elems;                 # 1 (was 0)
G.parse("ab"); G.parse("ab");
say @log.elems;                 # 1 (was 2: once per parse)
```

A regex literal runs from its source tree, so the phaser lift (#10550) could rewrite the tree.
A declaration's matcher parses the pattern *text*, which still spelled the phaser, so the phaser
ran each time the match reached the code block. The lift now edits both: it lifts the
statement-level `INIT`/`CHECK` phasers out of the declaration's tree, then respells the matching
code blocks in the pattern text so each reads the lifted phaser's slot
(`src/runtime/phasers/decl_regex.rs`). A declaration whose phasers it cannot account for
statement by statement keeps them where they were.

Not covered yet: a phaser nested in a statement of the code block (`{ if $x { INIT ... } }`)
and `BEGIN` in a declaration body.
(#12748)
