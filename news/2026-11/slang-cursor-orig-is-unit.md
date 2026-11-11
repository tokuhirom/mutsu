# A slang production's `$/.orig` is the whole unit; `Grammar.parse(:pos)`

A slang production ran in the slang host on the text from the production to the
end of the unit, so its cursor's `.orig` lacked everything before it and `.from`
was always 0. EEL 0.3 places each definition it compiles by counting the
newlines of `$/.orig.substr(0, $/.from)`, so every definition reported line 1
(`t/04-slang.t`, "definition records source line", #16888).

The parser now sends the host the unit's text and the production's character
offset in it (`unit_text_at`), and the host matches there with
`subparse(:pos)`, so `.orig`, `.from` and `.to` are the unit's, as rakudo's
cursor has them; the positions the host reports and asks about are translated
back for the parser. When the parser is reading a copy of the text (a heredoc's
leaked region) the old form is kept.

On the way, two `Grammar` bugs: `.subparse(:$pos)` / `pos => $p` ignored the
position (the value arrived in its item container and was not read through),
and `.parse(:pos(N))` ignored it altogether; `.parse` now matches from `N` and
still has to reach the end.
