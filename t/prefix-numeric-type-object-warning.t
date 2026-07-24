use v6;
use Test;

# Prefix `+`/`-` (numeric coercion / negation) on a bare type object emits the
# resumable "Use of uninitialized value of type X in numeric context" warning
# and resumes with the type's own numeric *zero* (Int -> 0, Num -> 0e0,
# Rat -> 0.0, FatRat -> FatRat.new(0,1), Complex -> 0+0i), matching rakudo.
# Previously mutsu silently resumed a bare Int 0 with no warning.

plan 25;

# --- prefix + resumes with the per-type zero (under `quietly`) --------------
{
    is (quietly +Int).raku,    '0',                 '+Int  -> 0';
    is (quietly +Num).raku,    '0e0',               '+Num  -> 0e0';
    is (quietly +Rat).raku,    '0.0',               '+Rat  -> 0.0';
    is (quietly +FatRat).raku, 'FatRat.new(0, 1)',  '+FatRat -> FatRat.new(0, 1)';
    is (quietly +Complex).raku, '<0+0i>',           '+Complex -> 0+0i';
    is (quietly +Real).raku,   '0',                 '+Real -> 0 (Int)';
    is (quietly +Cool).raku,   '0',                 '+Cool -> 0 (Int)';
    is (quietly +Any).raku,    '0',                 '+Any  -> 0 (Int)';
    is (quietly +Str).raku,    '0',                 '+Str  -> 0 (Int)';
}

# --- resumed value is defined and correctly typed ---------------------------
{
    my $n = quietly +Num;
    is $n.^name, 'Num', '+Num resumes with a Num';
    ok $n.defined, 'the resumed zero is defined';
}

# --- prefix - resumes with the negated per-type zero ------------------------
{
    is (quietly -Int).raku, '0',    '-Int -> 0';
    is (quietly -Num).raku, '-0e0', '-Num -> -0e0 (negative zero)';
    is (quietly -Rat).raku, '0.0',  '-Rat -> 0.0';
    is (quietly -Any).raku, '0',    '-Any -> 0';
    is (quietly -Str).raku, '0',    '-Str -> 0';
}

# --- the warning actually fires (it is a resumable warning, so `quietly`
#     suppresses it and CONTROL can catch it) ------------------------------
{
    my $warned = False;
    { +Int; CONTROL { default { $warned = True; .resume } } }
    ok $warned, 'prefix + on a type object emits a control warning';
}
{
    my $warned = False;
    { -Int; CONTROL { default { $warned = True; .resume } } }
    ok $warned, 'prefix - on a type object emits a control warning';
}

# --- a user Numeric method dispatches even on the bare type object ----------
{
    my class Foo { method Numeric { 42 } }
    is +Foo, 42, '+Foo dispatches the user Numeric method (no warning)';
    is -Foo, -42, '-Foo dispatches then negates';
}

# --- Mu still hard-errors (no Numeric candidate) ----------------------------
dies-ok { my $x = +Mu }, '+Mu is a hard error';
dies-ok { my $x = -Mu }, '-Mu is a hard error';

# --- defined numeric values are completely unaffected -----------------------
is +42, 42,     '+42 unaffected';
is +4.5, 4.5,   '+4.5 unaffected';
is -(2+3i), -2-3i, 'negating a defined Complex is unaffected';
