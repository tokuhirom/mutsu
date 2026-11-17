use Test;

# `my ($a, $b) = EXPR` (#15737): a call, method call or array-variable
# right-hand side compiles to `OpCode::ListAssignRhs` instead of a by-name
# call of the `__mutsu_list_assign_rhs` helper, and the declaration's result
# list is not built when the statement is sunk. These pin the right-hand-side
# shapes on both sides of that line, and every place the declaration's value
# is still observed.

plan 16;

sub two() { 1, 2 }
sub itemized() { $[3, 4] }
sub one-pair() { a => 1 }
my $cell = [5, 6];
sub rw-ret() is rw { $cell }

{
    my ($a, $b) = two();
    is-deeply ($a, $b), (1, 2), 'a call returning a List';
}
{
    my ($a, $b) = itemized();
    is-deeply ($a, $b), (3, 4), 'an itemized Array from a call is deitemized';
}
{
    my ($a, $b) = one-pair();
    is $a.raku, ':a(1)', 'a Pair returned by a call is the first element';
    is $b.raku, 'Any', '... and nothing is left for the second';
}
{
    my ($a, $b) = rw-ret();
    is-deeply ($a, $b), (5, 6), 'an is-rw routine returning an Array container';
}
{
    my ($a, $b) = 'x y z'.words;
    is-deeply ($a, $b), ('x', 'y'), 'a method call returning a Seq';
}
{
    my @src = 7, 8, 9;
    my ($a, @rest) = @src;
    is $a, 7, 'an array variable: first target';
    is-deeply @rest, [8, 9], '... the slurpy target takes the rest';
}
{
    my ($a, $b) = 1 xx *;
    is-deeply ($a, $b), (1, 1), 'an infinite right-hand side is not forced';
}

# The declaration's value where it is observed.
sub tail-decl() { my ($a, $b) = 10, 20 }
is-deeply tail-decl(), (10, 20), "a routine's last statement returns the list";
my @z = (my ($p, $q) = two());
is-deeply @z, [1, 2], 'in expression position the value is the targets';
ok (my ($u, $v) = two()), 'as a condition it is the (true) list';

# Statement position, inside a hot loop (JIT-compiled after a few
# iterations): the result list is skipped, the targets still assigned.
sub name-at(Str $s, Int $from) {
    my int $probe = $from;
    $probe++ while $probe < $s.chars && $s.substr($probe, 1) ne ' ';
    ($s.substr($from, $probe - $from), $probe);
}
my $acc = '';
for ^200 {
    my ($name, $after) = name-at('ab cd', 0);
    my ($n2, $a2) = name-at('ab cd', $after + 1);
    $acc = "$name|$after|$n2|$a2";
}
is $acc, 'ab|2|cd|5', 'a sunk declaration in a hot loop';

# A target read before its own declaration finishes still sees the default
# (the predeclaration is untouched).
my $x = 5;
{
    my ($x, $y) = $x, 2;
    is $x.raku, 'Any', 'the right-hand side sees the new, still-unset $x';
}

# A slip right-hand side keeps the helper call.
{
    my ($a, $b) = |two();
    is-deeply ($a, $b), (1, 2), 'a slipped right-hand side';
}
{
    my %h = k => [1, 2];
    my ($a, $b) = %h<k>;
    is-deeply ($a, $b), (1, 2), 'an element read keeps the helper call';
}
