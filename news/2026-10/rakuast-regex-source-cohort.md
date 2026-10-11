# RakuAST retains numeric regex aliases and quoted assertion atoms

The regex source tree now preserves numbered and aggregate subrule capture
aliases, nested assertion prefixes such as `<?!alpha>`, low Unicode quote pairs,
and smart double-quoted interpolation. Quote boundaries share the existing interpolation-aware scanner,
so quotes inside a method argument do not terminate the containing regex atom.

Separated quantifiers preserve the whitespace on their atom and separator when
rebuilt from RakuAST. Aliases retain the complete quantified body, and numeric
aliases render as `$N=` so the matcher keeps its static capture-slot numbering.
The existing compiler and matcher execute the reconstructed tree.

The parser helpers are split into bounded atom parsing, aliases, lookarounds
and quotes. Nested atoms and assertion-prefix chains are limited to 128 rather
than growing without a bound. The regression cohort checks source structure,
constructed trees and execution against Rakudo and both mutsu frontends.
