# MIDI::Make: anonymous class operand, `Buf.splice` on a temporary, `»**»` precedence

MIDI::Make's `t/all.rakutest` died at its first block and, as each gap was
closed, at the next. Three independent gaps, each fixed generally:

- `class :: { ... }.new xx 2` compiles its operand more than once, which reached
  the compile-time class-redeclaration check twice for the one parser-minted
  `__ANON_CLASS_<n>__` name. That name is unique by construction and is now
  exempt from the check.
- `Buf.splice` on a receiver no variable holds (`Buf.write-uint32(...).splice(1)`,
  an attribute's `Buf`) answered "No such method". The by-value row now splices
  through the buffer's shared storage and returns the removed bytes; the
  argument rules are one function shared with the named-receiver path.
- The hyper `**` (`»**»`, `«**«`, ...) now has `**`'s precedence, tighter than a
  prefix operator: `^256 »**» 2` is `^(256 »**» 2)` (65536 elements), where it was
  `(^256) »**» 2` (256 squares), which made the `Pow2` subset reject 8.

Pinned by `t/oo/class/anon-class-in-repeated-operand.t`,
`t/types/buf-splice-on-temporary.t` and
`t/lang/operators/hyper-pow-precedence-over-prefix.t`. MIDI::Make
`t/all.rakutest` now matches raku (50 ok).
