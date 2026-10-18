# `$seq eqv Any` no longer consumes the Seq

`eqv` reified/consumed both operands before comparing, so `my $s = (1,2).Seq; $s eqv Any;`
left `$s` dead (`X::Seq::Consumed` on the next read). A type-object operand is answered
without iterating the other side, as in Rakudo. Data::Summarizers' predicate chain
(`$_ eqv Any` over an array of Seqs) now works; `t/03-summarize-over-iterables-of-iterables.rakutest`
matches Rakudo.
