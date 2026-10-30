# A `:=` alias of a loop parameter no longer freezes it under an ENTER phaser

`for @a -> $b { my $x := $b; ENTER { ... }; ... $b ... }` read `$b` as its
first value on every iteration. The ENTER phaser's block scope re-synced the
alias targets of every `:=`-bound local of the chunk on exit, including `$x`
from the previous pass, which was not declared yet on this one, and wrote its
stale value back into `$b`. The sync now skips locals declared after the
exiting block. This is the loop shape of the `annotations` distribution's
`Buffer.STAMP`.
