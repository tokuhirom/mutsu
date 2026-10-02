use Test;

# Assigning Nil to an untyped scalar resets it to Any even when the store
# reaches the variable by name rather than through a local slot: a routine or
# closure writing a captured `$x`, an `our` variable, and the run-time half of
# a class/module body that a BEGIN in the file made the prologue split
# (#10608). `$/` and `$!` default to Nil and keep it.

plan 12;

{
    my $x = 5;
    sub f { $x = Nil }
    f;
    is $x.raku, 'Any', 'a sub writing a captured $x';
}

{
    my $x = 5;
    my &c = { $x = Nil };
    c();
    is $x.raku, 'Any', 'a closure writing a captured $x';
}

{
    our $y = 5;
    sub g { $y = Nil }
    g;
    is $y.raku, 'Any', 'a sub writing an our variable';
}

{
    class A { my $x = 5; method m { $x = Nil; $x.raku } }
    is A.m, 'Any', 'a method writing a class-body lexical';
}

{
    # Named apart from the other tests' `$x`: `is default` is still keyed
    # by name and leaks into a same-named split class body (#10796).
    my $d is default(3) = 5;
    sub h { $d = Nil }
    h;
    is $d, 3, 'is default(...) still wins';
    my Int $i = 5;
    sub k { $i = Nil }
    k;
    is $i.raku, 'Int', 'a typed scalar resets to its type';
}

{
    sub specials { $/ = Nil; $! = Nil; $_ = Nil; ($/.raku, $!.raku, $_.raku) }
    is-deeply specials(), ('Nil', 'Nil', 'Any'), '$/ and $! keep Nil, $_ resets to Any';
}

# A BEGIN anywhere in the file splits each package body into a BEGIN-time
# declaration and a run-time part.
my @seen;
class B { my $x = Nil; @seen.push: $x.raku }
class C { my $x = 5; $x = Nil; @seen.push: $x.raku }
module M { my $x = Nil; @seen.push: $x.raku }
BEGIN 1;
is @seen[0], 'Any', 'class body: my $x = Nil';
is @seen[1], 'Any', 'class body: $x = Nil';
is @seen[2], 'Any', 'module body: my $x = Nil';

{
    my $x = 5;
    my $r = do { sub { $x = Nil }() };
    ok $r === Nil || $r === Any, 'the assignment still yields a value';
    is $x.raku, 'Any', 'an anonymous sub writing a captured $x';
}
