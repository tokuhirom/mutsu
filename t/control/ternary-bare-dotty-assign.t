use Test;

plan 4;

# A bare `.=meth` binds at method-postfix precedence, so a ternary branch
# may hold it without parentheses (it used to die with "Precedence of = is
# too loose to use inside ?? !!").
{
    $_ = "a";
    my $r = 1 ?? .=uc !! 2;
    is $_, "A", 'bare .= in the then branch writes the topic';
    is $r, "A", 'and yields the new value';
}
{
    $_ = "b";
    my $r = 0 ?? 1 !! .=uc;
    is $_, "B", 'bare .= in the else branch writes the topic';
}
{
    $_ = "c";
    throws-like { EVAL '1 ?? $_ = $_.uc !! 2' }, Exception,
        'a written assignment still needs parentheses';
}
