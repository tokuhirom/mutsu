use Test;


plan 4;

my $n = 5;
my $count; BEGIN $count = 0;
multi trait_mod:<is>(Mu:U \c, :$marked!) { $count++ ; say "# {c.^name} sees {$n.raku}" }

sub f { our class K is marked { } }

is $count, 2, 'both traits ran before the mainline, without calling the routines';
sub g { our class L is marked { } }
f();
f();
g();
is $count, 2, 'calling the routines does not run the traits again';
ok K.^name eq 'K', 'the class exists';
ok L.^name eq 'L', 'the second class exists';
