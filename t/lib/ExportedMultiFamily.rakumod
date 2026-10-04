unit module ExportedMultiFamily;

# A proto exported before its candidates: each later candidate joins the
# family's export stash as it is declared.
proto sub describe(|) is export {*}
multi sub describe(Int $x) { "Int $x" }
multi sub describe(Str $x) { "Str $x" }
multi sub describe(Int $x, Int $y) { "Int,Int $x $y" }
multi sub describe(Rat $x) { "Rat $x" }
multi sub describe() { "nothing" }

# Every candidate carries its own `is export`, with a tag only some carry.
multi sub shape(Int $x) is export { "int-shape $x" }
multi sub shape(Str $x) is export { "str-shape $x" }
multi sub shape(Int $x, Str $y) is export(:DEFAULT, :extra) { "pair-shape $x $y" }
multi sub shape(Num $x) is export { "num-shape $x" }

# Candidates declared before the first exported one are part of the family
# too: exporting a multi exports its whole dispatcher.
multi sub late(Int $x) { "late-int $x" }
multi sub late(Str $x) { "late-str $x" }
multi sub late(Num $x) is export { "late-num $x" }
multi sub late(Bool $x) { "late-bool $x" }
