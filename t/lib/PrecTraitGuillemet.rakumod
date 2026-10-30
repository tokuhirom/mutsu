# Shell::DSL shape: `|>>` is declared tighter than a trait-less `|>` whose name
# holds a `>`, spelled with guillemets.
unit module PrecTraitGuillemet;
sub infix:«|>»(*@o) is assoc<list> is export { "pipe(" ~ @o.join(",") ~ ")" }
sub infix:«|>>»($a, $b) is assoc<non> is tighter(&infix:«|>») is export { "app($a,$b)" }
