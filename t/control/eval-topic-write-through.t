use v6;
use Test;

plan 8;

# An EVAL unit runs in its caller's scope, so an assignment to the topic is
# visible afterwards (#13298), while the unit's tail value is not published
# through `$_`.
$_ = "a"; EVAL q[$_ = "b"]; is $_, "b", 'EVAL assigns the caller topic';
$_ = "a 2"; EVAL q[s[\d] += 5]; is $_, "a 7", 's[] += in EVAL rewrites the caller topic';
$_ = "a 3"; EVAL q[s/\d/9/]; is $_, "a 9", 's/// in EVAL rewrites the caller topic';
$_ = "a 3"; EVAL q[.=uc]; is $_, "A 3", '.= in EVAL rewrites the caller topic';
$_ = 3; EVAL q[$_++]; is $_, 4, '$_++ in EVAL is not undone by the tail value';
$_ = "keep"; EVAL q[1 + 2]; is $_, "keep", 'a plain tail value leaves the topic alone';
$_ = "keep2"; EVAL q[for 1..2 { }]; is $_, "keep2", 'a for loop restores the topic';
$_ = "keep3"; is EVAL(q[my $x = 4; $x]), 4, 'the EVAL value is still the tail';
