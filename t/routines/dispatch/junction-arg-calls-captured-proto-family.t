use Test;

# From the Red distribution: Red::AST::Optimizer::AND calls the imported
# `compare` multi with `any(...)` arguments. A code value captured from a
# `proto`/`multi` family whose family has since grown reaches the family by
# name; a Junction argument that no candidate takes whole must still thread.

plan 5;

role AST { }
class A does AST { has $.v }
proto compare($, $) {*}
multi compare(AST $a, AST $b) { $a.v == $b.v }

my &c = &compare;
multi compare(Int $a, Int $b) { $a == $b }   # the family grows after the capture

my ($x, $y) = A.new(v => 1), A.new(v => 2);
is-deeply compare(any($x, $y), $x).Bool, True, "by-name call threads over any()";
ok c(any($x, $y), $x) ~~ Junction, "captured dispatcher call returns a Junction";
ok c(any($x, $y), $x).Bool, "captured dispatcher: any(True, False) is true";
nok c(all($x, $y), $x).Bool, "captured dispatcher: all(True, False) is false";
ok c(any(1, 2), 1).Bool, "grown candidate threads too";
