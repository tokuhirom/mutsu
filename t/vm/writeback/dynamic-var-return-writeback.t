use Test;

# A routine's write to a caller's dynamic variable is visible after it
# returns, for a `*`-twigil variable and for a twigil-less `is dynamic` one,
# while an ordinary lexical of the caller is left alone (#10110). Expected
# values from rakudo.

plan 4;

sub bump-twigil { $*count++ }
{
    my $*count = 1;
    bump-twigil() for ^3;
    is $*count, 4, 'a $* variable written by a callee';
}

sub bump-caller { $CALLER::n++ }
{
    my $n is dynamic = 10;
    bump-caller();
    is $n, 11, 'a twigil-less is dynamic variable written via CALLER::';
}

sub local-only { my $plain = 99; $plain }
{
    my $*flag = 'x';
    my $plain = 1;
    local-only();
    is $plain, 1, 'a same-named callee lexical does not leak back';
    is $*flag, 'x', 'an untouched dynamic keeps its value';
}
