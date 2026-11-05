unit module RxDefUnitMatcher;
sub match-in-module($s, $re) is export { $s ~~ $re }
sub bool-in-module($s, $re) is export { so $s ~~ $re }
