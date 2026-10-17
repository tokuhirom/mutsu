unit module UseInBlockSetOpLexical;

# User candidates of the built-in set operators, for
# t/modules/import-export/use-in-block-binds-set-operators-lexically.t
class Bx is export { has $.n }
multi sub infix:<(|)>(Bx $a, Bx $b) is export { Bx.new(n => $a.n + $b.n) }
multi sub infix:<(elem)>(Bx $a, Bx $b) is export { 'custom-elem' }
