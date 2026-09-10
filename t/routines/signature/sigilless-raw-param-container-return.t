use Test;

# ADR-0067 Slice 1: a SIGILLESS lexical (`\x`) names a storage location exactly
# as a `$`-sigiled one does, so an `is rw` / `is raw` routine whose tail is a
# bare sigilless name hands its caller that name's container.
#
# The parser spells a sigilless name `Expr::BareWord`, not `Expr::Var`, so the
# container-mode tail compile never recognised it and the routine returned a
# decontainerized value. Every expectation below was verified against
# raku v2026.07.

plan 21;

# --- A. a sigilless parameter's container survives the return ---------------
{
    sub f(\x) is raw { x }
    my $a = 42;
    f($a) = 5;
    is $a, 5, '`is raw` routine with a sigilless-name tail assigns through';
}
{
    sub f(\x) is rw { x }
    my $a = 42;
    f($a) = 5;
    is $a, 5, '`is rw` routine with a sigilless-name tail assigns through';
}
{
    sub f(\x) { return-rw x }
    my $a = 42;
    f($a) = 5;
    is $a, 5, 'an explicit `return-rw` of a sigilless name assigns through';
}

# --- B. the returned thing really is a container ----------------------------
{
    sub f(\x) is raw { x }
    my $a = 42;
    is f($a).VAR.^name, 'Scalar', 'the call result reports a Scalar container';
    my $b := f($a);
    $b = 7;
    is $a, 7, 'binding the result and writing through reaches the source';
    $a = 11;
    is $b, 11, 'the binding sees a later write to the source (one shared cell)';
}

# --- C. an ordinary read still decontainerizes ------------------------------
{
    sub f(\x) is raw { x }
    my $a = 42;
    my $c = f($a);
    $c = 7;
    is $a, 42, 'a plain `my $c = f($a)` copies the value, it does not alias';
}

# --- D. the container comes from whatever produced the argument -------------
{
    sub f(\x) is raw { x }
    my @a = 1, 2;
    f(@a[0]) = 9;
    is-deeply @a, [9, 2], 'a sigilless parameter over an array element';
}
{
    sub f(\x) is raw { x }
    my %h = a => 1;
    f(%h<a>) = 9;
    is %h<a>, 9, 'a sigilless parameter over a hash element';
}

# --- E. several parameters, and a computed tail -----------------------------
{
    sub f(\x, \y) is raw { y }
    my $a = 1;
    my $b = 2;
    f($a, $b) = 9;
    is $a, 1, 'the parameter that was NOT returned is untouched';
    is $b, 9, 'the returned parameter is written through';
}
{
    sub f(\x, \y, $c) is raw { $c ?? x !! y }
    my $a = 1;
    my $b = 2;
    f($a, $b, True) = 9;
    is $a, 9, 'a ternary tail assigns through the taken branch';
    is $b, 2, 'and leaves the other branch alone';
}

# --- F. the method form -----------------------------------------------------
{
    class C { method m(\x) is rw { x } }
    my $a = 42;
    C.new.m($a) = 5;
    is $a, 5, 'an `is rw` METHOD with a sigilless-name tail assigns through';
}

# --- G. a routine that is not rw-capable still refuses ----------------------
{
    sub f(\x) { x }
    my $a = 42;
    dies-ok { f($a) = 5 }, 'a plain sub with a sigilless tail is not assignable';
    is $a, 42, 'and the source is unchanged';
}

# --- H. a bareword that is NOT a lexical must not be boxed ------------------
# `Expr::BareWord` is also how a type name and an enum value are spelled; only a
# bareword that resolves to a local slot denotes a container.
{
    my @a = (Int, Str);
    is @a.map(*.^name).join(','), 'Int,Str', 'a list of type names is unaffected';
    my $p = (a => Int);
    is $p.value.^name, 'Int', 'a fat-arrow pair with a type-name value is unaffected';
}
{
    my \y = 5;
    is y, 5, 'an ordinary sigilless declaration still reads as its value';
}
{
    my $v = 1;
    my \z := $v;
    z = 9;
    is $v, 9, 'a sigilless binding to a scalar still aliases it';
}
{
    sub f() is raw { my \w = 5; w }
    is f(), 5, 'a routine returning an inline sigilless declaration reads back';
}
