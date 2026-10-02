# A class-body `:=` bind aliases the outer lexical

`my $y = 1; class D { my \x := $y; method m { x } }; $y = 7; say D.new.m`
printed `1`; Rakudo prints `7` (#10682). The sigiled `my $w := $z` had the
same problem, and a write through the alias reached the outer variable but was
computed from the stale read.

Three things kept the two names apart:

- A class body compiles each statement as its own chunk, so every
  `my $w := $z` was block-final, and the block-final declaration arm stored a
  copy of the right-hand side instead of binding it. It now routes a `:=`
  declaration through the statement compile, which emits the bind.
- The chunk reaches `$z` only by name, so the bind had no cell to share and
  fell back to a by-name alias that forwarded writes only. Class registration
  now boxes the declaring frame's slots that body binds name
  (`CompiledClassDeclPlan::body_bind_source_slots`) into shared cells before
  the body runs.
- A declaration bind whose source is reached by name and already lives in a
  cell now binds to that cell, as a package-qualified source already did.

The body static, the outer variable and the methods that capture the static
now share one container.
