# A derived attribute with another sigil shadows the base accessor

`class P { has $.v = "p" }; class Q is P { has @.v = <q1 q2> }; Q.new.v` answered the
base class's `"p"`. The construction order treated `$.v` and `@.v` as different
attributes; a redeclaration now replaces the base slot by its bare name whatever the
sigil, and a same-named attribute of any sigil gets per-class storage (#15281).
