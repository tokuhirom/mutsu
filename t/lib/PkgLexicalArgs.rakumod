unit module PkgLexicalArgs;

constant $LETTERS = 'абвгґд'.comb.List;
my $pair = <a b>;

sub in-letters($c) is export { so $c eq any $LETTERS }
sub pair-info() is export { (elems($pair), reverse($pair).join, sort($pair).join) }
