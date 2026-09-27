use v6;
use Test;

plan 10;

# A `gather` body runs in the environment it captured from the enclosing scope.
# Two things leaked through it:
#
# - the body's last statement value was delivered through `$_` (the
#   mainline/EVAL result convention), overwriting the enclosing `$_`;
# - an element topic (`given %h<k>` / `take $_ with %h<k>`) looked its
#   container up by name, which missed the captured outer lexical and
#   topicalized Nil.
#
# Game::Entities' `gather .view(*).each: -> \e { take .value with .get: e, X }`
# lost its registry topic after the first such gather.

{
    $_ = 'outer';
    my @a = gather { my $v = 42 };
    is $_, 'outer', 'a gather body tail value does not become $_';
}

{
    $_ = 'outer';
    my @a = gather { take 1; my $v = 46 };
    is $_, 'outer', '... also after a take';
}

{
    $_ = 'outer';
    my @a = lazy gather { my $v = 43 };
    my $x = @a[0];
    is $_, 'outer', '... also when the gather is forced later';
}

{
    sub one(&c) { c(1) }
    $_ = 'outer';
    my @a = gather { one(-> \e { 1 with e }) };
    is $_, 'outer', 'a closure called from the body keeps its topic to itself';
}

{
    sub each-of(&c) { c($_) for 1..2 }
    $_ = 'outer';
    my @a = gather { each-of(-> \e { 1 }) };
    is $_, 'outer', 'a sub looping over its own topic does not leak it';
}

{
    class Registry {
        has %.h;
        method get($k) { %!h{$k} }
        method each(&c) { c($_) for 1..3 }
    }
    with Registry.new(h => {1 => 'a', 3 => 'c'}) {
        my @x = gather .each: -> \e { take $_ with .get: e };
        my @y = gather .each: -> \e { take $_ with .get: e };
        is-deeply [|@x, |@y], [<a c a c>], 'the enclosing topic survives two gathers';
    }
}

{
    my %h = 1 => 'a';
    is-deeply (gather { take $_ with %h{1} }).List, ('a',),
        'a `with` element topic reads the captured hash';
    is-deeply (gather { given %h{1} { take $_ } }).List, ('a',),
        'a `given` element topic reads the captured hash';
    my %n = a => {b => 2};
    is-deeply (gather { take $_ with %n<a><b> }).List, (2,),
        'a chained element topic reads the captured hash';
    my @arr = 5;
    is-deeply (gather { given @arr[0] { take $_ } }).List, (5,),
        'an array element topic reads the captured array';
}
