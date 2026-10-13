use Test;

# A multi family with an inline `where` takes the light call path once its
# winner has run (ADR-12529 §2.4): the cached entry runs only the family's
# stage program and goes to the winner's light bind. This file pins that the
# winner, the redispatch chain and the number of `where` evaluations are what
# the generic path gives. Every expectation was checked against rakudo. The
# resolution count is pinned by tests/multi_call_resolves_once.rs.

plan 6;

{
    my @log;
    multi f(Int $x where * > 0) { @log.push('pos'); callsame() ~ '+' }
    multi f(Int $x) { @log.push('int'); 'int' }
    multi f(Str $x) { 'str' }
    is (^6).map({ f($_ - 2) }).join(','), 'int,int,int,int+,int+,int+',
        'the where winner and the wider candidate alternate by value';
    is @log.join(','), 'int,int,int,pos,int,pos,int,pos,int',
        'callsame from the cached where winner reaches the wider candidate';
    is f('a'), 'str', 'another type key still dispatches by type';
}

{
    multi g(Int $x where * %% 2) { nextsame }
    multi g(Int $x) { "next:$x" }
    is (^4).map({ g($_) }).join(','), 'next:0,next:1,next:2,next:3',
        'nextsame from the cached where winner';
}

{
    multi h(Int $x where * > 0) { samewith($x - 1) ~ '!' }
    multi h(Int $x) { 'base' }
    is h(3), 'base!!!', 'samewith re-dispatches through the family';
}

# At file scope: declared inside a block, the `where` currently runs twice
# per call on every path (#13183).
my $n = 0;
sub c() { $n++; 0 }
multi k(Int $x where * > c()) { 'pos' }
multi k(Int $x) { 'int' }
my @r = (^5).map({ k($_) });
is "{@r.join(',')} $n", 'int,pos,pos,pos,pos 5', 'the where runs once per call';
