use Test;

plan 2;

my $stored;
my $reached = False;
sub failure() { fail 'boom' }
sub target() { 7 }

{
    CATCH { default { } }
    my &target = { $stored = failure() };
    target();
    $reached = True;
}

ok $reached, 'a sunk lexical block call does not sink its stored Failure';
is $stored.^name, 'Failure', 'the lexical block still ran';
