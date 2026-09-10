use Test;

# VM-native default construction extended to typed `$`-scalar attributes
# (ledger §1 / phase ③: native `.new` without the interpreter bridge). The
# native path handles the pure-data cases (provided values that type-match,
# typed defaults, untyped attrs); anything needing the interpreter's richer
# semantics (type mismatch, uninitialized typed attr -> type object, coercion,
# native int/num types, required/where/`@`/`%` attrs) falls through. These
# assertions all match `raku`.

plan 30;

# --- provided typed value + typed default --------------------------------
{
    class P { has Int $.x; has Int $.y = 10 }
    my $p = P.new(x => 5);
    is $p.x, 5, 'provided typed Int attribute';
    is $p.y, 10, 'typed attribute default';
    is $p.x.^name, 'Int', 'provided value keeps its type';
}

# --- uninitialized typed attribute is the type object (Int), not Nil -----
{
    class P2 { has Int $.x; has Int $.y = 10 }
    my $q = P2.new;
    is $q.x.^name, 'Int', 'uninitialized typed attr is the type object';
    nok $q.x.defined, '... and is undefined';
    is $q.y, 10, '... while a defaulted typed attr is filled';
}

# --- Str / Real / Cool typed attributes ----------------------------------
{
    class S { has Str $.s = "hi" }
    is S.new.s, 'hi', 'Str typed default';
    is S.new(s => "yo").s, 'yo', 'provided Str value';
}
{
    class R { has Real $.v = 1.5 }
    is R.new.v, 1.5, 'Real typed default';
    is R.new(v => 3).v, 3, 'Int provided for a Real attribute (Int ~~ Real)';
}

# --- mixed typed + untyped attributes ------------------------------------
{
    class Mixed { has Int $.n; has Str $.t = "z"; has $.u }
    my $m = Mixed.new(n => 3, u => [1, 2]);
    is $m.n, 3, 'mixed: provided typed Int';
    is $m.t, 'z', 'mixed: typed default';
    is-deeply $m.u, [1, 2], 'mixed: untyped attribute';
    my $m2 = Mixed.new(n => 7);
    is $m2.n, 7, 'mixed: only typed provided';
    is $m2.t, 'z', 'mixed: default still applied';
    nok $m2.u.defined, 'mixed: unprovided untyped attr is undefined';
}

# --- inheritance with typed attributes -----------------------------------
{
    class Animal { has Str $.name = "?" }
    class Dog is Animal { has Int $.age = 3 }
    my $d = Dog.new(name => "Rex", age => 5);
    is $d.name, 'Rex', 'inherited typed attr provided';
    is $d.age, 5, 'child typed attr provided';
    my $d2 = Dog.new;
    is $d2.name, '?', 'inherited typed default';
    is $d2.age, 3, 'child typed default';
}

# --- typed default referencing an earlier attribute ----------------------
{
    class Calc { has Int $.a = 2; has Int $.b = 3; has Int $.c = $!a + $!b }
    my $c = Calc.new;
    is $c.c, 5, 'typed default referencing earlier typed attrs';
    my $c2 = Calc.new(a => 10);
    is $c2.c, 13, 'typed default sees a provided earlier attr';
}

# --- subset-typed attribute (type_matches_value handles the where) -------
{
    subset Even of Int where * %% 2;
    class Box { has Even $.e = 4 }
    is Box.new.e, 4, 'subset-typed default';
    is Box.new(e => 8).e, 8, 'subset-typed provided value that matches';
}

# --- type mismatch falls through to the interpreter and dies -------------
# (exact exception type is the interpreter's concern; assert it dies.)
{
    class T { has Int $.x }
    dies-ok { T.new(x => "not an int") }, 'wrong-typed provided value dies';
}
{
    class Num1 { has Int $.x = 1 }
    dies-ok { Num1.new(x => "str") }, 'wrong-typed value with siblings dies';
}

# --- native lowercase types still go through the interpreter -------------
# `has int $.x` defaults to 0 (not a type object); native path bails -> works.
{
    class Nat { has int $.x }
    is Nat.new.x, 0, 'native int attr defaults to 0 (interpreter path)';
    is Nat.new(x => 42).x, 42, 'native int attr provided';
}

# --- repeated construction in a loop -------------------------------------
{
    class Pt { has Int $.x = 0; has Int $.y = 0 }
    my @pts = (^4).map({ Pt.new(x => $_, y => $_ * 2) });
    is-deeply @pts.map(*.x).Array, [0, 1, 2, 3], 'loop construction: x values';
    is-deeply @pts.map(*.y).Array, [0, 2, 4, 6], 'loop construction: y values';
}
