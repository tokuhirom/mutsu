# RakuAST marker classes and `Term::Enum` have constructors

`RakuAST::Parameter::Slurpy::{Flattened,Unflattened,SingleArgument,Capture}`,
`RakuAST::Regex::Backtrack::{Frugal,Greedy,Ratchet}` and `RakuAST::Term::Enum` were the last classes mutsu's
`.AST` emits that failed with "Could not find symbol '&Name'" on `.new`. A marker's `.new` now answers the
type object itself, as on rakudo, and `Term::Enum.new(Name)` builds the node `from-identifier` builds.
An explicitly passed empty `ArgList` is now left out of `.raku` like an absent one, and an empty
`Signature.new` renders without parentheses. Pinned by `t/rakuast/rakuast-marker-class-constructors.t`.
