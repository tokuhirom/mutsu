# RakuAST: regex declarations with comments, spaced modifiers and quantified `~` goals

Regex declarations whose body the source tree could not read refused to cross
the RakuAST boundary ("regex declaration without a source tree"). The tree now
reads, and renders as rakudo does:

- comments inside a regex (`# line`, `` #`(embedded) ``), which count as written
  whitespace;
- a spaced backtrack modifier (`'a' : 'b'`);
- a `~` construct whose expression carries a quantifier and separator
  (`'(' ~ ')' <k>+ % ','`);
- `<|w>`, the same node as `<.wb>`;
- captures and anchors inside a lookaround (`<?before $<sp>=' '+>`) and an
  aliased negated lookahead (`$<a>=<!before y>`);
- code blocks with a comment holding quotes or braces, or a nested regex literal
  with `<["']>`.

A single-quoted literal is spelled with single quotes again when it needs no
escape, so `FAILGOAL` keeps reporting a `~` goal in its source form.

Along the way the runtime regex scanners learned that `\x[2B]` inside a
character class does not close it: `/<[\x[2B]]> [a|4]/` and `/<[\x[2B] a]>/` were
errors. 39 more `t/` files pass under `MUTSU_RAKUAST=1`.
