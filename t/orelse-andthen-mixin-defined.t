use Test;

# `orelse`/`andthen`/`notandthen` (OpCode::CallDefined) must honor a
# user-defined `.defined` method on a role-composed mixin (`but role {...}`),
# just like `//` (JumpIfNotNil) does. The CallDefined path only looked up a
# class-MRO `.defined` (Instance/Package), missing a role override.

plan 7;

# --- Mixin whose role reports it undefined --------------------------------
{
    my Int $i = 1 but role { method defined { False } };
    is ($i orelse "z"), "z", 'orelse runs RHS when the mixin role .defined is False';
    is ($i andthen "yes"), Nil, 'andthen skips RHS when the mixin role .defined is False';
    is ($i notandthen "no"), "no", 'notandthen runs RHS when the mixin role .defined is False';
}

# --- Mixin whose role reports it defined ----------------------------------
{
    my Int $i = 5 but role { method defined { True } };
    is ($i orelse "z"), 5, 'orelse keeps the value when the mixin role .defined is True';
    is ($i andthen $_ + 1), 6, 'andthen runs RHS (with topic) when defined is True';
}

# --- A mutating `.defined` on the mixin runs exactly once per test --------
{
    my $calls = 0;
    my $i = 1 but role { method defined { $calls++; True } };
    $i andthen 1;
    $i andthen 1;
    is $calls, 2, 'the mixin .defined override runs once per andthen (caller lexical mutation propagates)';
}

# --- Instance (class-MRO) override still works via the existing path ------
{
    class C { method defined { False } }
    my $c = C.new;
    is ($c orelse "y"), "y", 'orelse honors a class instance .defined override';
}
