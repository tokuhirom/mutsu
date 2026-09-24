use v6;
use Test;

# `$x ~= <Str>` appends in place for a closure-captured variable (the
# AtomicCompoundVar cell path) and for a private attribute (`$!s`, fused into
# ConcatAssignLocal) -- #9209. Both used to copy the whole accumulated string
# on every append. The in-place path must never be observable: aliases keep
# their value, constraints still apply and leave the old value on failure,
# undefined and non-Str operands behave as before, and concurrent appends
# through one captured cell lose nothing.

plan 18;

# -- closure-captured ---------------------------------------------------------

{
    my $x = "";
    my &g = { $x ~= "ab" };
    g() for ^3;
    is $x, 'ababab', 'captured: appends accumulate';
    my $y = $x;
    g();
    is "$x|$y", 'abababab|ababab', 'captured: a copy taken before the append keeps its value';
}

{
    my $x = "e";
    my &g = { $x ~= "\x[301]" };
    g();
    is $x.chars, 1, 'captured: a combining suffix composes with the join';
}

{
    my Str $x = "s";
    my &g = { $x ~= "t" };
    g();
    is $x, 'st', 'captured: a Str-typed variable';
}

{
    my subset Short of Str where .chars < 4;
    my Short $x = "ab";
    my &g = { $x ~= "cd" };
    dies-ok { g() }, 'captured: a subset constraint still rejects the result';
    is $x, 'ab', 'captured: and the old value is kept';
}

{
    my $x;
    my &g = { $x ~= "z" };
    g();
    is $x, 'z', 'captured: an undefined variable seeds the empty string';
}

{
    my $x = 5;
    my &g = { $x ~= "z" };
    g();
    is $x, '5z', 'captured: a non-Str value stringifies';
}

{
    my $x = "";
    await (^4).map: { start { for ^250 { $x ~= "q" } } };
    is $x.chars, 1000, 'captured: concurrent appends through one cell lose nothing';
}

# -- private attribute --------------------------------------------------------

{
    my class C { has $.s = ""; method go($n) { $!s ~= "ab" for ^$n; self } }
    is C.new.go(3).s, 'ababab', 'attribute: appends accumulate';
    my $c = C.new;
    my $keep = $c.go(1).s;
    $c.go(1);
    is "{$c.s}|$keep", 'abab|ab', 'attribute: a value read through the accessor keeps its value';
}

{
    my class T { has Str $.s = ""; method go { $!s ~= "x"; self } }
    is T.new.go.go.s, 'xx', 'attribute: a Str-typed attribute';
}

{
    my subset Tiny of Str where .chars < 3;
    my class W { has Tiny $.s = "a"; method go { $!s ~= "bc"; self } }
    my $w = W.new;
    dies-ok { $w.go }, 'attribute: a subset constraint still rejects the result';
    is $w.s, 'a', 'attribute: and the old value is kept';
}

{
    my class R { has $.s is rw = "r"; method go { $!s ~= "s"; self } }
    my $r = R.new;
    my $alias := $r.s;
    $r.go;
    is "$alias|{$r.s}", 'rs|rs', 'attribute: an alias bound to an rw attribute sees the append';
}

{
    my class U { has $.s; method go { $!s ~= "u"; self } }
    is U.new.go.s, 'u', 'attribute: an undefined attribute seeds the empty string';
}

{
    my class E { has $.s = "e"; method go { $!s ~= "\x[301]"; self } }
    is E.new.go.s.chars, 1, 'attribute: a combining suffix composes with the join';
}

{
    my class N { has $!s = ""; method go { $!s ~= "n" for ^3; $!s } }
    is N.new.go, 'nnn', 'attribute: a private attribute with no accessor';
}
