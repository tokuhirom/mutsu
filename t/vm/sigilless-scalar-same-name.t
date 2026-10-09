use Test;

plan 8;

# A sigilless binding `\t` and a scalar `$t` are different symbols (#11994).
sub nested(\t) { sub { my $t = 5; $t + t }() }
is nested(10), 15, 'nested closure: my $t does not hide the \t parameter';

sub same-frame(\t) { my $t = 5; $t + t }
is same-frame(10), 15, 'same frame';

sub in-block(\t) { my $f = { my $t = 5; $t + t }; $f() }
is in-block(10), 15, 'block closure';

sub captured(\t) { my $t = 5; my $f = { $t + t }; $f() }
is captured(10), 15, 'closure nested inside the shadowing scope';

sub method-case(\t) {
    my class C { method m { my $t = 5; $t + t } }
    C.new.m
}
is method-case(10), 15, 'method in an enclosing sub';

my \u = 7;
{ my $u = 3; is $u + u, 10, 'my \u and $u in an inner block'; }
is u, 7, 'the sigilless term is intact after the block';

sub after(\x) { { my $x = 2; $x += x; }; x }
is after(40), 40, 'bare read after the shadowing scope ends';
