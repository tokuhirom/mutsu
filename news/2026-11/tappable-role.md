# The core `Tappable` role can be composed by user classes

`class M does Tappable { ... }` died with `X::InvalidType: Invalid typename
'Tappable'`: the name was known as a core type but no role stood behind it.
Rakudo's `Tappable` (the base of `Supply`'s tap protocol) declares `tap`,
`live` and `sane` as required methods and a default `serial` that returns the
invocant. It is now supplied as a real role prelude (injected into any unit
that mentions the name, modules included), so a composing class that omits a
required method fails to compile with rakudo's message and the rest composes.

Found by the EEL distribution, whose `MatcherSupply` composes it. EEL itself
still stops at the slang activation (a parametric grammar role mixed into
`MAIN`), which is part of the slang-by-execution campaign.
