use Test;

plan 4;

sub infix:<op> ($a, $b) { $a - $b }
{
    sub infix:<op> ($a, $b) { $a ** $b }
    is &infix:<Rop>(2, 3), 9, "Rop autogen uses block-local user-defined operator";
}
is &infix:<Rop>(2, 3), 1, "Rop autogen restores outer user-defined operator";

is-deeply (1 R, 2 R, 3 R, 4), (4, 3, 2, 1), "R comma preserves list-associative reversal";

my $got;
sub with-named (:$value) { $got = $value }
with-named value => 3 R- 2;
is $got, -1, "named argument receives reversed metaop value";
