# The compiled regex engine no longer hands calls or leaf atoms to the walk

Some atoms answer at most one end:

- builtin calls such as `<alpha>`, `<ws>`, `<wb>` and `<:Letter>`
- backreferences
- `<(` / `)>`
- `$x`
- `"…"`
- `<{ … }>`
- `<~~>`

The compiled regex engine used to match these through the old tree walk's single-atom matcher.
Their definitions now live in a module of their own, `regex_atom_leaf.rs`. Both engines call it.

The last `<name>` calls that were bridged to the walk now run on the compiled engine:

- wrapped tokens
- grammars under a custom metaclass
- rules whose `"…"` atoms interpolate lexicals
- calls whose arguments no candidate binds

One behavior changed. A rule whose `"…"` atom interpolates a lexical is now resumed lazily, as in
rakudo. Code inside it runs once per end the caller backtracks to, where it used to run once per
end the rule could produce.

Over `t/grammar`, `t/regex` and `t/modules`, walk leaf uses drop by 1872 and bridged calls
drop from 54 to 0. This is ADR-0135 §8, Slice E, part twenty.
