unit module MG;
class P is export { has $.v }
our constant G is export = P.new(v => 7);
multi infix:<*>(Int $n, G) is export { "const-G" }
multi infix:<*>(Int $n, P:D $p) is export { "generic" }
