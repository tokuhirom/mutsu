# Accessor-returned List refuses element assignment

`$obj.a[0] = x` where `@!a` was rebound to an immutable `List` (`@!a := @!a.List`) wrote
through the accessor and mutated the attribute; it now raises `X::Assignment::RO` like
Rakudo. Found via Markdown::Lex (`t/01-blocks.rakutest`, `t/03-fences.rakutest`), whose
`Table` and `CodeFence` blocks are immutable by this mechanism.
