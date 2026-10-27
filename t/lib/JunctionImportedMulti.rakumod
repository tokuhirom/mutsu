# Multis a distribution (Red's `compare`) exports: typed candidates behind an
# untyped `proto`, none of which takes a Junction.
unit module JunctionImportedMulti;

role Node is export { }
class Leaf does Node is export { has $.n }

proto same($, $) is export {*}
multi same(Node $a, Node $b) { $a.n == $b.n }
multi same(Leaf $a, Int $b) { 'leaf-int' }

multi both(Node $a, Node $b) is export { $a.n + $b.n }
multi both(Leaf $a, Int $b) is export { 'leaf-int' }

multi takes-any(Mu $a) is export { 'mu' }
multi takes-any(Int $a) is export { 'int' }
