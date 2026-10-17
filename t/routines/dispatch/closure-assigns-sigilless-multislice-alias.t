use Test;

# A closure that assigns a sigilless parameter bound to a slice writes
# through to the sliced container, as the same assignment in the routine
# body does (#13730). Checked against rakudo.

plan 4;

my %hash = a => { b => { c => 42, d => 666, e => 1 } };
sub called-block(\target, \values) { { target = values }() }
called-block(%hash{"a";"b";<c d e>}, (777, 888, 999));
is-deeply %hash, { a => { b => { c => 777, d => 888, e => 999 } } },
    'a block called in place writes a multi-dim slice';

my %flat = a => 1, b => 2;
sub named-closure(\t, \v) { my &f = { t = v }; f() }
named-closure(%flat<a b>, (8, 9));
is-deeply %flat, { a => 8, b => 9 }, 'a named closure writes a hash slice';

sub escaping(\x) { my &c = { x = 5 }; &c }
my $v = 1;
escaping($v)();
is $v, 5, 'an escaping closure still writes a scalar alias';

sub direct(\target, \values) { target = values }
my %d = p => 1, q => 2;
direct(%d<p q>, (3, 4));
is-deeply %d, { p => 3, q => 4 }, 'the routine body\'s own assignment is unchanged';
