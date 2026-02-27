use Test;

plan 2;

sub add($a, $b);
is add(1, 2), 3, 'forward declaration resolves to later body';
sub add($a, $b) { $a + $b }

sub sum4($$$$);
is sum4(1, 2, 3, 4), 10, 'compact anonymous-sigil signature works in forward declaration';
sub sum4($$$$) {
    my ($a, $b, $c, $d) = @_;
    $a + $b + $c + $d
}
