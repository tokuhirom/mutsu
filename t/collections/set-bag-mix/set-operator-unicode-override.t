use Test;

class UnicodeBox { }
multi sub infix:<∪>(UnicodeBox $a, UnicodeBox $b) { 'unicode' }
multi sub infix:<∩>(UnicodeBox $a, UnicodeBox $b) { 'unicode' }
multi sub infix:<⊎>(UnicodeBox $a, UnicodeBox $b) { 'unicode' }
multi sub infix:<⊍>(UnicodeBox $a, UnicodeBox $b) { 'unicode' }
multi sub infix:<∖>(UnicodeBox $a, UnicodeBox $b) { 'unicode' }
multi sub infix:<⊖>(UnicodeBox $a, UnicodeBox $b) { 'unicode' }
my $u = UnicodeBox.new;
is ($u ∪ $u), 'unicode', '∪ selects its own candidate';
is ($u (|) $u).^name, 'Set', '(|) ignores the Unicode candidate';
is ($u ∩ $u), 'unicode', '∩ selects its own candidate';
is ($u (&) $u).^name, 'Set', '(&) ignores the Unicode candidate';
is ($u ⊎ $u), 'unicode', '⊎ selects its own candidate';
is ($u (+) $u).^name, 'Bag', '(+) ignores the Unicode candidate';
is ($u ⊍ $u), 'unicode', '⊍ selects its own candidate';
is ($u (.) $u).^name, 'Bag', '(.) ignores the Unicode candidate';
is ($u ∖ $u), 'unicode', '∖ selects its own candidate';
is ($u (-) $u).^name, 'Set', '(-) ignores the Unicode candidate';
is ($u ⊖ $u), 'unicode', '⊖ selects its own candidate';
is ($u (^) $u).^name, 'Set', '(^) ignores the Unicode candidate';
{
    my $copy = $u;
    $copy ∪= $u;
    is $copy, 'unicode', '∪= selects the Unicode candidate';
}
{
    my $copy = $u;
    $copy ∩= $u;
    is $copy, 'unicode', '∩= selects the Unicode candidate';
}
{
    my $copy = $u;
    $copy ⊎= $u;
    is $copy, 'unicode', '⊎= selects the Unicode candidate';
}
{
    my $copy = $u;
    $copy ⊍= $u;
    is $copy, 'unicode', '⊍= selects the Unicode candidate';
}
{
    my $copy = $u;
    $copy ∖= $u;
    is $copy, 'unicode', '∖= selects the Unicode candidate';
}
{
    my $copy = $u;
    $copy ⊖= $u;
    is $copy, 'unicode', '⊖= selects the Unicode candidate';
}
done-testing;
