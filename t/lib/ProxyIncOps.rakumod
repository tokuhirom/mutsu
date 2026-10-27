# Found by Red 0.2.7: the increment candidates reach the importer through an
# EXPORT map, not the routine table.
class Col { has $.n }
my @seen;
multi postfix:<++>(Col $a) { @seen.push: $a.n; $a }
sub seen() { @seen }

sub EXPORT() {
    Map.new: ('Col' => Col, '&postfix:<++>' => &postfix:<++>, '&seen' => &seen)
}
