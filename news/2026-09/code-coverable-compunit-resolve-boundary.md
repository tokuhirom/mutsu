# Code::Coverable reaches the CompUnit resolver boundary

The ecosystem roulette measured `Code::Coverable` 0.0.13 against current
`main` (locked on [#8977](https://github.com/tokuhirom/mutsu/issues/8977)).
Its load probe passes, and Rakudo passes `t/01-basic.rakutest` with 1/1
assertions, but mutsu reaches `Identity::Utils`'s bytecode lookup and then
dies because `CompUnit::Repository::FileSystem.resolve` is not implemented.

The failure is the deep CompUnit handle/precompilation gap tracked by
[#9004](https://github.com/tokuhirom/mutsu/issues/9004), not a bounded
distribution-specific fix. The other test file,
`t/02-identity.rakutest`, is `no_baseline`: Rakudo itself dies before its
first assertion. The refreshed ledger record therefore remains red at 0/1
baseline files, with the remaining mutsu regression explicitly accounted for
by #9004.
