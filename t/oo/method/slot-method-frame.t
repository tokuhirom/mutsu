use Test;

# ADR-18393 stage 1: a leaf method (slot reads, constants and arithmetic
# only) runs in a slot frame with no Env overlay, readonly scope or fat
# call-frame record. These pin what such a frame must still get right.

plan 16;

class N { has $.v; method Numeric { $!v } }
multi sub infix:<+>(N $a, Int $b) { "plus:" ~ $a.v }

class C {
    has $.x = 5;
    method m0() { 1 }
    method m3($a) { $a + 1 }
    method m4($a, $b) { $a * $b - 2 }
}

my $o = C.new;
is $o.m0, 1, 'constant body';
is $o.m3(2), 3, 'one parameter';
is $o.m4(3, 4), 10, 'two parameters';
is $o.m3(N.new(v => 7)), 'plus:7', 'arithmetic that calls a user operator';

{
    my $r = try { $o.m3("abc") };
    isa-ok $!, X::Str::Numeric, 'an exception thrown in the body propagates';
    is $o.m3(41), 42, 'the frame is usable again after an exception';
}

{
    my $a = 10;
    is $o.m3($a), 11, 'argument named like the parameter';
    is $a, 10, 'the caller variable is untouched';
    my $c = 1;
    is $o.m3($c) + $c, 3, 'caller slots are intact after the call';
}

sub through-a-sub { my $s = 2; $o.m3($s) * $s }
is through-a-sub(), 6, 'called from a sub frame';

is-deeply (1..5).map({ $o.m3($_) }).List, (2, 3, 4, 5, 6), 'called from a closure';

{
    my @r = await (^4).map: -> $t {
        start { my $s = 0; $s += $o.m3($_) for ^1000; $s }
    };
    is-deeply @r, [500500 xx 4], 'concurrent calls';
}

role R { method rm($a) { $a + 100 } }
class D does R { }
is D.new.rm(1), 101, 'role method composed into a class';

{
    my $*dyn = 7;
    multi sub infix:<->(N $a, Int $b) { $*dyn }
    class E { method e($x) { $x - 1 } }
    is E.new.e(N.new(v => 1)), 7, 'a callee of the body sees the caller dynamics';
}

{
    my $w;
    CONTROL { when CX::Warn { $w = .message; .resume } }
    class F { method f($x) { $x - 1 } }
    is F.new.f(Any), -1, 'an undefined argument warns and numifies';
    like $w, /uninitialized/, 'the warning is raised in the slot frame';
}
