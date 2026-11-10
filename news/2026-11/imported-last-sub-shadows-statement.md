# An imported `sub last` shadows the `last` statement

ParaSeq exports `sub last` to learn that a mapped block ended early, then calls
`&CORE::last()`. mutsu always parsed a bare `last` / `next` / `redo` as the
loop-control statement, so the exported routine never ran and ParaSeq never
stopped the other batches. A routine of that name that is declared or
imported now takes the statement's place (bare or called), and
`&CORE::last()` / `&CORE::next()` / `&CORE::redo()` raise the real loop
control even when such a routine is in scope. ParaSeq's `t/47-deepmap`,
`t/49-nodemap` pass.
