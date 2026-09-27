# Never imports OpScope::Where, so its `*` is always the core operator.
unit module OpScope::Plain;
sub plain-mul($a, $b) is export { $a * $b }
