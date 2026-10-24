unit module lowercasepkg::Types;
class Fact is export { has $.r }
sub unknown(Str $r --> Fact) is export { Fact.new(:$r) }
