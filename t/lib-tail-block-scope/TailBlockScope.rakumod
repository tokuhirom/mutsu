unit module TailBlockScope;
sub plain($a, $b) is export { $a - $b }
# The last statement is a bare block: its routines are lexical to it.
{ multi infix:<->(UInt $a, UInt $b) is export { "user" } }
