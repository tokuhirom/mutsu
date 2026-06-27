use Test;

plan 5;

# The implicit special variables `$_`, `$/`, `$!` are dynamic by nature
# (no `*` twigil, but `.VAR.dynamic` is True), unlike ordinary lexicals.

ok  $/.VAR.dynamic, '$/ is dynamic';
ok  $_.VAR.dynamic, '$_ is dynamic';

# An explicit `*`-twigil dynamic is still dynamic.
{
    my $*d = 1;
    ok $*d.VAR.dynamic, 'explicit $*d is dynamic';
}

# An ordinary lexical is NOT dynamic.
{
    my $x = 1;
    nok $x.VAR.dynamic, 'ordinary lexical $x is not dynamic';
}

# A non-dynamic lexical accessed through CALLER still throws (regression guard
# for the X::Caller::NotDynamic path, which also consults is_var_dynamic).
throws-like 'sub f() { CALLER::<$y> }; my $y; f',
    X::Caller::NotDynamic,
    'non-special lexical through CALLER still throws NotDynamic';
