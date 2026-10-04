use Test;

# A subset `where` predicate that fails by throwing (`fail "msg"`) reports its
# own message from the type check it rejected -- and from that check only.

plan 6;

subset S of Int where { $_ > 0 or fail "custom S" };
subset T of S where { $_ < 10 };

{
    my $msg = (try { my S $a = 0 }) // $!.message;
    is $msg, 'custom S', 'assignment to a subset reports the where-fail message';
}
{
    my $msg = (try { my T $b = 0 }) // $!.message;
    is $msg, 'custom S', "a subset of a subset reports its base's where-fail message";
}
{
    my $msg = (try { my S:D $c = 0 }) // $!.message;
    is $msg, 'custom S', 'a smiley-qualified subset reports the where-fail message';
}
{
    my $msg = (try { my T $d = 20 }) // $!.message;
    like $msg, /'expected T but got Int'/, "the outer subset's plain rejection is generic";
}

# The rejection of an earlier, unrelated check must not leak into a later
# type-check error.
try { my $ = 0 ~~ S };
{
    my $msg = (try { my Str $s = 3 }) // $!.message;
    like $msg, /'expected Str but got Int'/, 'a failed smartmatch leaves no stale where-fail behind';
}
{
    my $msg = (try { my T $e = 20 }) // $!.message;
    like $msg, /'expected T but got Int'/, 'a later passing base predicate clears nothing stale';
}
