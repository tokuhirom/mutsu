# A bare name installed by `sub EXPORT` wins over a same-named tag-exported sub

`use Terminal::ANSI::OO :t; t.color(...)` died with "Unknown function: t"
(#9339; Syntax::Highlighters and Terminal::MultiProgress both do this). The
module declares `sub t is export(:t)` inside its class *and* a
`sub EXPORT($t = 't') { %( $t => Terminal::ANSI::OO.new ) }` hook that installs
a sigilless term under the same name. In Raku a bare `t` names that term; only
`t()` calls the sub.

mutsu's static module scan learns `t` is a routine from the tagged
`sub t is export(:t)` (deliberately regardless of tags, ADR-0087), so the
importer's parser compiled the bare `t` -- in `t.color`, `t;`, `my $x = t` --
as a zero-arg call. The term the hook actually installed is not knowable at
parse time (it can depend on the `use` arguments), so the call was all there
was, and with a plain `use` the tagged sub is not even imported.

The fix follows the shape #9047 used for `True`/`False`: in a compunit that
imported through an `EXPORT` hook, a bare, paren-less, argument-less use of an
imported routine name now parses to `Expr::ExportTermOrCall`, which compiles to
the new `OpCode::GetExportTermOrJump` followed by the ordinary call. At run time
the opcode pushes the term when the hook installed one under that name
(`export_term_override_names`, the same gate `GetShadowableTerm` uses) and
skips the call; otherwise execution falls through into the call. Compunits that
imported nothing through a hook, and `t()` with parentheses, are unchanged.

Pinned by `t/modules/import-export/export-hook-term-shadows-tagged-sub.t` and
`...-imported.t`. While writing them, three shapes where a tag-exported sub of
an `EXPORT`-hook module is not imported at all turned up (a block-scoped
`use M :tag`, a later re-`use`, and a sub nested in a class body); they are
filed as #9389.
