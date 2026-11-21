# A type declared in a module block shadows an outer type of the same name

`class G { }; module M { class G { }; say G.^name }` printed `G`: mutsu bound
the short name of a nested type lexically only when the enclosing package was
a class. In a module or package block the bare name kept meaning the outer
type, for `our class`, plain `class`, `my class`, `my package` and roles alike,
both in the block and in the block's routines called later.

A type or package declared in a module or package block now binds its short
name in that block, and is recorded by its qualified name so the block's
routines still resolve it through the package they run in. Because the record
is keyed by `M::G`, a `class Other::G` declared outside `module Other` does
not shadow an outer `G` inside it, as on rakudo (#18481).
