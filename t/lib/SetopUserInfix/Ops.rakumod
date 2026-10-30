# The set operators a distribution (Red) overloads for its own result-set type,
# an `Iterable` role with a parameter.
unit module SetopUserInfix::Ops;

role Rs[::T] does Iterable is export {
    has $.name;
    method iterator { (1, 2).iterator }
    method union($o) { self.new(:name("({$.name} union {$o.name})")) }
}
class Row does Rs[Int] is export { }
class Rs2 is Row is export { }

multi infix:<(|)>(Rs $a, Rs $b) is export { $a.union: $b }
multi infix:<∪>(Rs $a, Rs $b) is export { $a (|) $b }
multi infix:<(&)>(Rs $a, Rs $b) is export { Row.new(:name("({$a.name} (&) {$b.name})")) }
multi infix:<∩>(Rs $a, Rs $b) is export { $a (&) $b }
multi infix:<(-)>(Rs $a, Rs $b) is export { Row.new(:name("({$a.name} (-) {$b.name})")) }
multi infix:<(+)>(Rs $a, Rs $b) is export { Row.new(:name("({$a.name} (+) {$b.name})")) }
