use Test;

# A closure's return carries back only the variables the closure itself
# writes (ADR-12529 §3, slice 43). A write that reaches the caller through a
# nested call, a dynamic or a shared cell still lands, and a closure built
# while a grammar subrule's argument runs still updates the caller's
# variable. Checked against rakudo.

plan 6;

{
    my $calls = 0;
    grammar G { token TOP { <x({ $calls++; 2 }())> }; token x($n) { a } }
    G.parse('a');
    is $calls, 1, 'a block called in a subrule argument';
}

{
    my $n = 0;
    my &bump = { $n++ };
    my &outer = { bump(); bump() };
    outer();
    is $n, 2, 'a closure whose nested call writes the variable';
}

{
    sub set-dyn() { $*D = 'set' }
    my $*D = 'orig';
    { set-dyn() }();
    is $*D, 'set', 'a closure whose nested call writes a dynamic';
}

{
    my $x = 'mine';
    sub own-x() { my $x = 'theirs'; $x }
    { own-x() }();
    is $x, 'mine', 'a routine the closure calls keeps its own same-named variable';
}

{
    my @log;
    my &rec = -> $v { @log.push($v) };
    { rec(1); rec(2) }();
    is-deeply @log, [1, 2], 'an array a nested closure pushes onto';
}

{
    my %seen;
    (1, 2, 1).map({ %seen{$_}++ }).eager;
    is-deeply %seen, { 1 => 2, 2 => 1 }, 'a hash a map block counts into';
}
