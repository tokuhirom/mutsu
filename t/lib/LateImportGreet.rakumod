unit module LateImportGreet;

sub late-greet(Str:D $s) is export { "hello $s" }
sub late-pair(Int:D $a, Int:D :$b = 1) is export { $a + $b }
