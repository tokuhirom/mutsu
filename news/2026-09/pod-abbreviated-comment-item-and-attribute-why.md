# Pod: `=comment` inline text, `=for itemN`, and `Attribute.WHY` docee binding

A random ecosystem draw landed on `Pod::TreeWalker` 0.0.7, whose suite was
3 of 5 files short of rakudo. All three gaps were in mutsu's Pod support:

- **Abbreviated `=comment text` dropped its inline text.** Only the lines
  *after* the directive were collected, so `=comment Trenchant` produced a
  comment block whose contents were `""` instead of `"Trenchant\n"`. The
  abbreviated form and `=for comment` now share one collector, in both the
  top-level and the nested Pod parser.
- **`=for item1` built a `Pod::Block::Named` called `item1`** instead of a
  `Pod::Item` of level 1, so list walkers never saw a list. Paragraph and
  delimited blocks now pick their class from the target name in one place
  (`headN` → `Pod::Heading`, `item`/`itemN` → `Pod::Item`, anything else
  `Pod::Block::Named`), and `=begin itemN`/`headN`/named blocks keep their
  config adverbs (`=begin item1 :numbered` used to lose `:numbered`).
- **`Attribute.WHY` now binds its docee, as Rakudo's does.** Rakudo's
  `Attribute.WHY` is `$!why.set_docee(self); $!why`: it returns the very
  `Pod::Block::Declarator` in `$=pod` and makes that block's `WHEREFORE` the
  attribute it was called on. mutsu built a fresh declarator block instead,
  so `$=pod[$i].WHEREFORE` never became the `.^attributes` object a walker
  compares against.

With these, all five `Pod::TreeWalker` test files pass under mutsu.
Regression test: `t/lang/pod-abbreviated-comment-and-item-blocks.t`.
