use Test;
plan 7;

# A user multi infix whose signature ties the narrowest core candidate is an
# ambiguous call, not a silent win for either side (#12973).
{
    multi infix:<%>(Int $a, Int $b) { 'user' }
    my ($a, $b) = 7, 3;
    throws-like { $a % $b }, X::Multi::Ambiguous, 'infix:<%>(Int, Int) ties the core candidate';
}
{
    multi infix:<+>(Int $a, Int $b) { 'user' }
    my ($a, $b) = 7, 3;
    throws-like { $a + $b }, X::Multi::Ambiguous, 'infix:<+>(Int, Int) ties the core candidate';
}
{
    multi infix:<+>(Int:D $a, Int:D $b) { 'user' }
    my ($a, $b) = 7, 3;
    throws-like { $a + $b }, X::Multi::Ambiguous, 'the :D spelling ties as well';
}
{
    # `is default` breaks the tie in the user candidate's favour.
    multi infix:<->(Int $a, Int $b) is default { 'dflt' }
    my ($a, $b) = 7, 3;
    is $a - $b, 'dflt', 'is default wins the tie';
}
{
    # A narrower candidate wins outright; other operand types still use core.
    multi infix:<*>(Int $a, Int $b where * > 0) { 'narrow' }
    my ($a, $b) = 7, 3;
    is $a * $b, 'narrow', 'a refined candidate out-ranks the core one';
    is 2.5 * 2, 5, 'other operand types still reach the core operator';
}
{
    multi infix:<%>(Str $a, Str $b) { 'str' }
    is 7 % 3, 1, 'an unrelated user candidate does not disturb Int % Int';
}
