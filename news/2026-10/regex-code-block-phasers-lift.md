# BEGIN, CHECK and INIT inside a regex code block run at their own phase

A phaser written in a regex code block (`/ a { INIT say "i" } /`) used to run when the
match reached the block, so it ran late, and once per match. Rakudo runs `BEGIN`, `CHECK`
and `INIT` of a regex literal with the enclosing unit's phasers, once.

The two phaser lifts (`src/runtime/phasers/lift.rs` and the nested-BEGIN walk) now descend
into a regex literal's code blocks like any other child closure. This is sound because the
tree they rewrite is the one a match executes: attaching a defining scope to a regex that
closes over a lexical used to drop the source tree and fall back to the pattern text (which
still spells the phaser); the tree now survives whenever the pattern interpolates no name.

Not covered yet: `token`/`rule` declarations and regexes with value-bearing adverbs
(`rx:i/.../`), whose matchers still run from the pattern text.
(#12748)
