use Test;

# The winner of a block's own `multi infix:<op>` family that a keyed dispatch
# program picked runs through the verified positional light bind (#18580,
# #18267 stage 4) when its body reads no variable of its declaring scope by
# name. The family's deferral, return-type check, recursion through the
# operator and dynamic variables behave as on the general call path. Expected
# values are rakudo's.
plan 8;

{
    my $*modulus = 1_000_003;
    multi infix:<*>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    multi infix:<+>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    my $acc = 1;
    for 1..2000 -> $i { $acc = $acc * $i + $i }
    is $acc, 221928, 'callsame and a dynamic variable over many operator calls';
}

{
    multi infix:<->(UInt $a, UInt $b --> UInt) { callsame() }
    is 5 - 3, 2, 'a return-typed winner returns its value';
    throws-like { 1 - 2 }, X::TypeCheck::Return, 'and its return type is checked';
}

{
    multi infix:<*>(UInt $a, UInt $b) { $a == 0 ?? 0 !! $b + ($a - 1) * $b }
    is 4 * 5, 20, 'a winner recursing through its own operator';
}

{
    multi infix:<~>(Str $a, Str $b where *.chars > 2) { "[" ~ nextsame() ~ "]" }
    is ("a" ~ "bcd", "a" ~ "b").join(' '), 'abcd ab', 'a where-constrained Str operand';
}

sub run-a($n) {
    my $*modulus = 7;
    multi infix:<+>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    (^$n).map({ $_ + 5 }).join(',')
}
sub run-b($n) {
    multi infix:<+>(UInt $a, UInt $b) { "b" ~ callsame() }
    (^$n).map({ $_ + 1 }).join(',')
}
is run-a(4), '5,6,0,1', 'a routine-local family';
is run-b(3), 'b1,b2,b3', 'another routine-local family of the same operator';
is 3 + 4, 7, 'outside, the built-in operator answers';
