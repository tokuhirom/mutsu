# Math::Polygons and Tinky::JSON: qualified type-object calls, Str smartmatch, ObjAt identity

Math::Polygons 0.0.6 was drawn from the ecosystem ledger at random. Two of its
five test files died with `Cannot dispatch to method Writer::serialize on XML
because it is not inherited or done by SVG`. The SVG module's
`class SVG is XML::Writer` calls `self.XML::Writer::serialize(...)`, and
Math::Polygons invokes it on the type object (`SVG.serialize(...)`). The
qualified-call path for non-instance invocants split the name at the *first*
`::`, so the qualifier became `XML` and the method `Writer::serialize`. That
path now splits at the last `::`, as the instance path already did, and all 5
files pass.

Tinky::JSON 0.0.8 died with the same error (`self.JSON::Class::from-json` on a
type object). With that fixed, it ran into three more gaps, all now fixed:

- `$obj ~~ "text"` ignored the object's own stringification. It is now
  `Str.ACCEPTS`, which compares against `.Stringy` (by default `.Str`) and
  treats a stringification that dies as a non-match, as Rakudo does.
- An ObjAt's own `.WHICH` was a fresh per-object identity, so two
  `$o.WHICH` values of the same object were different Set elements.
  `$o.WHICH.WHICH` is now `ObjAt|<$o.WHICH>`, and Set/Bag keys use the same
  string.
- `⊆`, `⊇`, `⊂` and `⊃` coerced a Junction operand into a one-element Set
  instead of autothreading over it. They now thread the way `∈` does, so
  `none(...) ⊆ any(...)` gives a Junction of per-pair answers.

The negated glyphs (`∉`, `⊈`, ...) still collapse a Junction before negating
it, because the parser rewrites them as `!(...)`. That is tracked separately.
