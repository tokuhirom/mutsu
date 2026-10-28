# nqp list ops work on an attribute a lexical was bound to

`my $its := $!its` shares the attribute's cell, so a later
`nqp::pop(nqp::getattr(self, F, '$!its'))` from another method received the
cell instead of the `IterationBuffer` and died with `expected a list,
IterationBuffer or Buf/Blob, got Any`. The shared backing-array helper now
reads through the cell. Found with ReverseIterables, whose `t/01-basic.rakutest`
now matches raku.
