use Test;

class ASCIIBox { }

multi sub infix:<(|)>(ASCIIBox $a, ASCIIBox $b) { 'ascii' }
multi sub infix:<(&)>(ASCIIBox $a, ASCIIBox $b) { 'ascii' }
multi sub infix:<(+)>(ASCIIBox $a, ASCIIBox $b) { 'ascii' }
multi sub infix:<(.)>(ASCIIBox $a, ASCIIBox $b) { 'ascii' }
multi sub infix:<(-)>(ASCIIBox $a, ASCIIBox $b) { 'ascii' }
multi sub infix:<(^)>(ASCIIBox $a, ASCIIBox $b) { 'ascii' }

my $a = ASCIIBox.new;
is ($a (|) $a), 'ascii', '(|) selects its own candidate';
is ($a ∪ $a).^name, 'Set', '∪ ignores the ASCII candidate';
is ($a (&) $a), 'ascii', '(&) selects its own candidate';
is ($a ∩ $a).^name, 'Set', '∩ ignores the ASCII candidate';
is ($a (+) $a), 'ascii', '(+) selects its own candidate';
is ($a ⊎ $a).^name, 'Bag', '⊎ ignores the ASCII candidate';
is ($a (.) $a), 'ascii', '(.) selects its own candidate';
is ($a ⊍ $a).^name, 'Bag', '⊍ ignores the ASCII candidate';
is ($a (-) $a), 'ascii', '(-) selects its own candidate';
is ($a ∖ $a).^name, 'Set', '∖ ignores the ASCII candidate';
is ($a (^) $a), 'ascii', '(^) selects its own candidate';
is ($a ⊖ $a).^name, 'Set', '⊖ ignores the ASCII candidate';
done-testing;
