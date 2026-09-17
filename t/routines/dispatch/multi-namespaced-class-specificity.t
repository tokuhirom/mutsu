use Test;

# A multi candidate typed with a namespaced (`Foo::Bar`) subclass must be
# ranked more specific than a sibling candidate typed with its namespaced
# parent — same rule as an unqualified class hierarchy, just spelled with
# `::`. `constraint_base_name` (used to derive the plain type name a multi
# candidate's constraint is ranked against) stopped scanning at the FIRST
# `:` byte it saw, which is also the first byte of a `::` package
# separator — so `"Foo::Base"` and `"Foo::Derived"` both truncated down to
# `"Foo"` and compared as equally (un)specific, letting the base class win
# regardless of declaration order. Found via the ecosystem
# XDG::GuaranteedResources / File::Directory::Tree distributions, whose
# `rmtree(Cool:D)` / `rmtree(IO::Path:D)` pair hit exactly this collision
# (`IO::Path does Cool`) and recursed forever.
plan 4;

class Foo::Base { }
class Foo::Derived is Foo::Base { }

multi sub pick-a(Foo::Base:D $x)    { "Base" }
multi sub pick-a(Foo::Derived:D $x) { "Derived" }
is pick-a(Foo::Derived.new), "Derived",
    "namespaced :D-typed multi picks the derived class, not the base";

multi sub pick-b(Foo::Base $x)    { "Base" }
multi sub pick-b(Foo::Derived $x) { "Derived" }
is pick-b(Foo::Derived.new), "Derived",
    "namespaced multi (no smiley) picks the derived class, not the base";

# Declaration order must not matter either way.
multi sub pick-c(Foo::Derived:D $x) { "Derived" }
multi sub pick-c(Foo::Base:D $x)    { "Base" }
is pick-c(Foo::Derived.new), "Derived",
    "namespaced multi specificity is independent of declaration order";

# A builtin example of the same collision: IO::Path does Cool, so
# `rmtree(Cool:D)` / `rmtree(IO::Path:D)` must prefer the narrower IO::Path
# candidate for an IO::Path argument instead of looping through the Cool
# candidate forever.
multi sub pick-d(Cool:D $x)    { "Cool" }
multi sub pick-d(IO::Path:D $x) { "IO::Path" }
is pick-d("/tmp".IO), "IO::Path",
    "IO::Path (which does Cool) beats a Cool:D sibling candidate";
