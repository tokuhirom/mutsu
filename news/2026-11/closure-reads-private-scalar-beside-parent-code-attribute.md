# A closure reads a private scalar attribute beside a same-named public code attribute

`-> $ { $!msg }` in a subclass whose parent declares `has &.msg` compiled to an
`AttrContainerRef("msg")`, which looked the bare key up first and handed back the parent's
callable. The promotion now prefers the sigil-qualified key a colliding scalar is stored under.
Test::Describe `t/04-change.rakutest` (`Match::Change`) now matches raku (#17272).
