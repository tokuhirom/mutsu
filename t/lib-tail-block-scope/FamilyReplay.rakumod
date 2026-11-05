unit module FamilyReplay;
sub tag($s) is export { "tag:$s" }
{
  multi infix:<+>(Str $a, Str $b) is export { "$a|$b" }
  multi infix:<->(Str $a, Str $b) is export { "$a~$b" }
}
