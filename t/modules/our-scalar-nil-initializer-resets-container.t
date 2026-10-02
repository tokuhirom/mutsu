use Test;

# Assigning `Nil` to a Scalar resets it to its default (`Any`, or the declared
# type's type object); it does not store a `Nil` in it. `my $x = Nil` always
# did that, but `our $x = Nil` kept the `Nil` itself, so `$x.raku` said `Nil`,
# `$x === Nil` was true -- and a BEGIN, which sees the static half of
# `our $m = 9` (a `Nil` initializer under the hood), read `Nil` instead of the
# container's default. Expected values are rakudo's.

plan 28;

# --- the untyped scalar ------------------------------------------------------
{
    our $v = Nil;
    is $v.raku, 'Any', '`our $v = Nil` holds the container default';
    ok $v.WHAT === Any, '... which is the Any type object';
    nok $v === Nil, '... and is not Nil';
    nok $v.defined, '... and is undefined';
    is $v.gist, '(Any)', '... and says (Any)';
}
{
    my $m = Nil;
    our $o = Nil;
    is $o.raku, $m.raku, '`our $o = Nil` agrees with `my $m = Nil`';
}

# --- a declared type ---------------------------------------------------------
{
    our Int $c = Nil;
    is $c.raku, 'Int', '`our Int $c = Nil` holds the Int type object';
    our Any $s = Nil;
    is $s.raku, 'Any', '`our Any $s = Nil` holds Any';
    our Mu $t = Nil;
    is $t.raku, 'Mu', '`our Mu $t = Nil` holds Mu';
    our Str $f = "x";
    $f = Nil;
    is $f.raku, 'Str', 'a later Nil assignment resets a typed `our` to its type object';
    our Int:D $d = 3;
    is $d, 3, 'a definite constraint with a real initializer is untouched';
}

# --- other initializers are stored as written --------------------------------
{
    our $n = Nil;
    $n = 3;
    is $n, 3, 'an `our` that held the default takes a later value';
    our $e = 5;
    $e = Nil;
    is $e.raku, 'Any', 'a later Nil assignment resets an untyped `our`';
    our $z = 0;
    is $z, 0, 'a falsy non-Nil initializer is stored as written';
    our $h is default(42) = Nil;
    is $h, 42, '`is default` still supplies its own value for a Nil initializer';
}

# --- scopes ------------------------------------------------------------------
{
    our $j = Nil;
    is $j.raku, 'Any', 'in a bare block';
}
{
    package P { our $k = Nil; }
    is $P::k.raku, 'Any', 'in a package, through its qualified name';
    package P2 { our $k2 = Nil; is $k2.raku, 'Any', 'in a package, through its own name' }
}
{
    class A { our $v = Nil; method m { $v.raku } }
    is A.m, 'Any', 'in a class, read by a method';
    is $A::v.raku, 'Any', 'in a class, through its qualified name';
}
{
    our $first = Nil;
    our $second = Nil;
    is ($first.raku, $second.raku).join(' '), 'Any Any', 'two declarations in one scope';
}

# --- a BEGIN sees the container default, not Nil ------------------------------
# At file scope, like the report: a BEGIN inside a nested block is lifted by a
# different mechanism (ADR-0134's nested-block residue) that this does not pin.
my $seen-m;
our $m = 9;
BEGIN $seen-m = $m.raku;
is $seen-m, 'Any', 'a BEGIN sees `our $m = 9` as the container default';
is $m, 9, '... and the run-time initializer still stores 9';

my $seen-b;
class B { our $v = 9 }
BEGIN $seen-b = $B::v.raku;
is $seen-b, 'Any', 'a BEGIN sees a class-body `our $v = 9` as the container default';
is $B::v, 9, '... and the class body still stores 9 at run time';

my $seen-t;
our Int $t2 = 4;
BEGIN $seen-t = $t2.raku;
is $seen-t, 'Int', 'a BEGIN sees a typed `our Int $t2 = 4` as its type object';
is $t2, 4, '... and the run-time initializer still stores 4';

my $seen-q;
our $q = 1;
BEGIN $seen-q = $q.defined;
is $seen-q, False, 'a BEGIN sees the static half of `our $q = 1` as undefined';
