use Test;

plan 5;

# The `.message` / `.Str` of these exceptions must match Rakudo exactly — no
# class-name prefix (that is `.gist`'s job), no extra explanatory suffix, and
# the offending type named bare (not gisted).

# X::Numeric::CannotConvert — no "; not a finite number" suffix.
{
    my $ex;
    try { Inf.Int };
    $ex = $!;
    is $ex.message, 'Cannot convert Inf to Int', 'CannotConvert message has no extra suffix';
}

{
    my $ex;
    try { NaN.Int };
    is $!.message, 'Cannot convert NaN to Int', 'CannotConvert NaN message';
}

# X::Mixin::NotComposable — the offending type is named bare (B, not (B)).
{
    class B { };
    try { 1 but B };
    is $!.message, 'Cannot mix in non-composable type B into object of type Int',
        'NotComposable names the type bare';
}

# X::TypeCheck::Binding — .message carries no class-name prefix.
{
    try { my Int $x := "foo" };
    is $!.message, 'Type check failed in binding; expected Int but got Str ("foo")',
        'Binding message has no class-name prefix';
    is $!.^name, 'X::TypeCheck::Binding', 'Binding exception type is correct';
}
