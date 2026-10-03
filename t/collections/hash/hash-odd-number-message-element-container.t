use Test;

# The "Odd number of elements" message shows the element it stopped at with
# `.raku`; an element container (an Array/Hash element, a `$`-scalar holder)
# shows the value it reads as, not the container's string form (#11112).
# Every expected message was measured on rakudo.

plan 5;

my $head = "Odd number of elements found where hash initializer expected:\n";
my %h = a => 1;

sub msg(&code) { code(); CATCH { default { return .message } }; 'lived' }

my @a; @a[0] = %h;
is msg({ my %c = (@a[0],) }), $head ~ 'Only saw: ${:a(1)}', 'an itemized array element';

my %k; %k<x> = %h;
is msg({ my %c = (%k<x>,) }), $head ~ 'Only saw: ${:a(1)}', 'an itemized hash value';

my $hi = %h;
is msg({ my %c = ($hi,) }), $head ~ 'Only saw: ${:a(1)}', 'a $-scalar holding a hash';

is msg({ my %c = (1, 2, @a[0]) }),
    $head ~ "Found 3 (implicit) elements:\nLast element seen: " ~ '${:a(1)}',
    'the last element of a longer list';

my @b = 1, 2; my $r := @b[1];
is msg({ my %c = ($r,) }), $head ~ 'Only saw: 2', 'a bound scalar element';
