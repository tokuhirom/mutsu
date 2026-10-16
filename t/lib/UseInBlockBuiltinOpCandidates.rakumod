unit module UseInBlockBuiltinOpCandidates;

class Pt is export { has $.v }
multi infix:<+>(Pt $a, Pt $b) is export { Pt.new(v => $a.v + $b.v) }
multi prefix:<->(Pt $a) is export { my $i = $a.v; Pt.new(v => -$i) }
