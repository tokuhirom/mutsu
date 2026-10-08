# Explicit `.list` on a reified Seq consumes it

`my $s = (1, 2, 3).map({ $_ }); $s.list; $s.List` now throws `X::Seq::Consumed`
as in raku. The parser used to lower `@$s` and an explicit `.list` to the same
method call, so the VM had to keep `.list` non-consuming on an already-reified
body for `@$s` to stay re-readable. The `@`-contextualizer forms are now tagged
`sugar: true`, and the compiler marks every other zero-argument `.list` call with
a new `ConsumeReifiedSeq` opcode that consumes the invocant Seq before dispatch.
Closes the ADR-0034 §7.1 known gap (#9930).

The RakuAST round trip (`MUTSU_RAKUAST=1`, the `ci/rakuast-frontend-passing.txt` ratchet) now
converts the sugared `.list` call to `Contextualizer::List` and lowers it back to the same sugared
call, so `@$s` stays the re-reading contextualizer there too.
