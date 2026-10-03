use Test;

# `Mu.new(*%attrinit)` takes named arguments only. Reached through
# `callwith`/`nextwith`/`nextsame` from a user `new`, it used to hand the
# arguments to `bless`, which silently dropped the positionals and built the
# object anyway (#9761). It must die exactly as a direct `C.new(1)` does.

plan 7;

{
    my class C1 { method new($x) { callwith($x) } }
    throws-like { C1.new(1) }, X::Constructor::Positional,
        'callwith a positional into the default constructor dies';
}
{
    my class C2 { method new($x) { nextwith($x) } }
    throws-like { C2.new(1) }, X::Constructor::Positional,
        'nextwith a positional into the default constructor dies';
}
{
    my class C3 { method new(|c) { nextsame } }
    throws-like { C3.new(1) }, X::Constructor::Positional,
        'nextsame with a positional into the default constructor dies';
}
{
    my class C4 { has $.a; method new($x) { callwith(a => $x) } }
    is C4.new(3).a, 3, 'named arguments still reach the default constructor';
}
{
    my class C5 { has $.a; method new(|c) { nextsame } }
    is C5.new(a => 5).a, 5, 'nextsame with named arguments still works';
}
{
    my class A is Array { method new(|c) { nextsame } }
    is A.new(1, 2).elems, 2, 'a builtin that takes positionals still gets them';
}
{
    my class V is Version { method new(|c) { nextsame } }
    is V.new("1.2").Str, "1.2", 'a builtin ancestor constructor still comes first';
}
