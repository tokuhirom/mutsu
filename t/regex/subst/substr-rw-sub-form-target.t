use Test;

# `substr-rw($t, ...) = v` (the sub form) writes into `$t` itself, even when
# another variable in scope holds an equal string. It used to find its target
# by scanning the env for an identical value and wrote into whichever variable
# came first (#9183).

plan 7;

my $s = "pqab";
{
    my $t = "pqab";
    substr-rw($t, 1, 1) = 'X';
    is $t, 'pXab', 'sub-form substr-rw writes into its own argument';
}
is $s, 'pqab', 'an outer variable holding an equal string is untouched';

my $u = "pqab";
my $w = "pqab";
substr-rw($w, 0, 2) = 'ZZ';
is $w, 'ZZab', 'the second of two equal strings is written';
is $u, 'pqab', 'the first of two equal strings is untouched';

my $e = "hello";
substr-rw($e, 0, 1) ~= '!';
is $e, 'h!ello', 'a compound assignment through the sub form';

my $b1 = Buf.new(1, 2, 3);
my $b2 = Buf.new(1, 2, 3);
subbuf-rw($b2, 0, 1) = Buf.new(9);
is-deeply $b2, Buf.new(9, 2, 3), 'sub-form subbuf-rw writes into its own argument';
is-deeply $b1, Buf.new(1, 2, 3), 'an equal Buf elsewhere is untouched';
