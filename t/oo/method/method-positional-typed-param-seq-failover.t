use v6;
use Test;

# Pod::To::HTML (ecosystem): a `Positional`-typed method parameter binds a
# Seq through PositionalBindFailover, exactly as a `Positional`-typed sub
# parameter does (rakudo#4864).
plan 8;

class A {
    method single(Positional $n) { "pos:" ~ $n.^name ~ ":" ~ $n.join(",") }
    multi method f(Positional $n --> Str) { "pos" }
    multi method f(Str $n --> Str) { "str" }
    multi method g($n --> Str) { "generic" }
    multi method g(Positional $n --> Str) { "pos" }
}

my $seq = (1, 2).map({ $_ * 2 });
is A.new.single($seq), "pos:List:2,4", 'single method: Seq binds to Positional';
is A.new.single([1, 2]), "pos:Array:1,2", 'single method: Array still binds';
is A.new.f((1, 2).map({ $_ })), "pos", 'multi method: Seq picks the Positional candidate';
is A.new.f("x"), "str", 'multi method: Str still picks Str';
is A.new.g((1, 2).map({ $_ })), "pos", 'Positional beats the untyped candidate for a Seq';
is A.new.g(5), "generic", 'untyped candidate still takes a non-Positional';
is A.new.g(1..3), "pos", 'Range still Positional';
ok !((1, 2).map({ $_ }) ~~ Positional), 'a Seq is still not Positional for smartmatch';
