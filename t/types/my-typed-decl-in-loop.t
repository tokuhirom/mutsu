use Test;

# A typed `my` declaration in a loop body re-registers its constraint on every
# execution, and that registration is memoized and partly skipped (#11467):
# the block-entry hoist is dropped when nothing runs before the declaration,
# the stale-constraint clear is skipped when the registration follows, a plain
# builtin constraint is resolved once per registry generation, and the type
# check runs JIT-compiled. Every observable behaviour must stay as it was.

plan 23;

throws-like { for ^3 { my Int $x = $_ == 2 ?? "a" !! $_ } },
    X::TypeCheck::Assignment, 'a leading typed declaration is checked on every iteration';

{
    my @r;
    for ^3 { my Str $s = "v$_"; @r.push: $s }
    is-deeply @r, ["v0", "v1", "v2"], 'a leading typed declaration stores every iteration';
}

throws-like { my Int $x = 1; for ^2 { my Str $x = "a" }; $x = "b" },
    X::TypeCheck::Assignment, 'a loop-body shadow leaves the outer constraint in place';

{
    my Int $x = 1;
    for ^2 { my Str $x = "a" }
    $x = 2;
    is $x, 2, 'the outer typed variable still accepts its own type';
}

{
    my $x = 1;
    for ^2 { my Str $x = "a" }
    $x = 42;
    is $x, 42, 'a loop-body typed shadow does not constrain an untyped outer';
}

throws-like { for ^2 { my $y = 1; EVAL q[$x = "s"]; my Int $x = 5 } },
    X::TypeCheck::Assignment, 'a statement before the declaration still sees its type';

{
    my $ok = True;
    for ^2 { my Str $s; $ok &&= $s === Str }
    ok $ok, 'an uninitialized typed scalar holds its type object';
}

{
    my $ok = True;
    for ^2 { my Str $s = "a"; $s = Nil; $ok &&= $s === Str }
    ok $ok, 'Nil assigned to a typed scalar resets it to the type object';
}

lives-ok { for ^3 { my Cool $c = $_; my Numeric $n = 1.5; my Any $a = "x" } },
    'values of a subtype pass a builtin constraint';

throws-like { for ^1 { my Int:D $x = Nil } },
    X::TypeCheck::Assignment, 'a :D declaration rejects a Nil initializer';

throws-like { for ^2 { my Str $s = 42 } },
    X::TypeCheck::Assignment, 'a mismatching value is still rejected';

{
    my $got;
    for ^2 { my Int() $x = "42"; $got = $x }
    is $got, 42, 'a coercion constraint still coerces';
    isa-ok $got, Int, 'the coerced value is an Int';
}

{
    for ^2 { my Int $x = 3 }
    my subset Small of Int where * < 10;
    lives-ok { for ^2 { my Small $s = 3 } }, 'a subset declared later is honoured';
    throws-like { for ^2 { my Small $s = 30 } },
        X::TypeCheck::Assignment, 'a subset declared later rejects values';
}

{
    sub f(::T $a) { for ^2 { my T $x = $a }; "ok" }
    is f(1) ~ f("s"), 'okok', 'a type capture constraint follows each binding';
    sub g(::T $a, $b) { my T $x = $b }
    throws-like { g(1, "s") }, X::TypeCheck::Assignment,
        'a type capture constraint rejects another type';
}

throws-like { for ^2 { my Int %h = a => 1; my Str @a = <x y>; %h<b> = "z" } },
    X::TypeCheck::Assignment, 'typed hash elements are still checked';

{
    my $keyof;
    for ^2 { my Int %h{Str} = a => 1; $keyof = %h.keyof }
    is $keyof, Str, 'an object hash declared in a loop keeps its key type';
}

{
    sub h { for ^2 { my Str $e = "x" }; my Str $e2 = "y"; $e2 }
    my $e = 1;
    h();
    $e = 5;
    is $e, 5, "a routine's typed lexical does not constrain the caller";
}

{
    my int $t = 0;
    for ^5 { my int $i = $_; $t += $i }
    is $t, 10, 'native int declarations in a loop';
    my $w;
    for ^2 { my int8 $i = 200; $w = $i }
    is $w, -56, 'a narrow native declaration still wraps';
}

{
    my $s = 0;
    for ^4 -> $i {
        my Int $x = $i;
        $s += $x;
    }
    is $s, 6, 'a typed declaration fed by the loop parameter';
}
