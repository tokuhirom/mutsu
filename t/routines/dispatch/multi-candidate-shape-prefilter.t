use Test;

# A multi candidate whose SHAPE cannot take the call (positional arity, a
# required named parameter nobody supplied, a named argument no parameter
# consumes) is refused before the type checks run (perf). The winners and the
# errors must be the ones the full matcher gave.
plan 25;

proto sub f(|) {*}
multi sub f(\a, $n)                              { "2" }
multi sub f(\a, $n, :$pos!)                      { "2+pos" }
multi sub f(\a, $n, **@also, :$pos!)             { "2+also+pos" }
multi sub f(\a, $n, :&cmp!, :$force)             { "2+cmp+force" }
multi sub f(\a, $n, **@also, :&cmp, :$force)     { "also+cmp+force" }
multi sub f(Int \a, Int:D $n, :$force!)          { "int+force" }

is f(1, 2), '2', 'two positionals';
is f(1, 2, :cmp(&infix:<cmp>)), '2+cmp+force', 'a required Callable named parameter';
is f(1, 2, :pos(3)), '2+pos', 'a required named parameter supplied';
is f(1, 2, :force), 'int+force', 'a typed candidate narrower than the untyped ones';
is f('a', 2, 3, :force), 'also+cmp+force', 'the untyped first parameter takes the slurpy candidate';
is f(1, 2, 3, 4, :force), 'also+cmp+force', 'a slurpy candidate takes extra positionals';
is f(1, 2, 3, :pos(9)), '2+also+pos', 'slurpy plus a required named';

# Aliased named parameters answer to either spelling.
multi sub g($x, :s(:$sort)) { "sort=" ~ ($sort // 'none') }
is g(1, :sort(5)), 'sort=5', 'the alias spelling';
is g(1, :s(6)), 'sort=6', 'the short spelling';
is g(1), 'sort=none', 'no named argument';

# A named slurpy swallows unknown named arguments.
multi sub h($x, *%rest) { join ',', %rest.keys.sort }
is h(1, :a, :b), 'a,b', 'a *%rest candidate takes any named argument';

# Unknown named argument, missing required named, wrong arity: no candidate.
multi sub k($x, :$known) { 'ok' }
dies-ok { EVAL 'k(1, :unknown)' }, 'an unconsumed named argument finds no candidate';
dies-ok { EVAL 'k()' }, 'too few positionals finds no candidate';
dies-ok { EVAL 'k(1, 2)' }, 'too many positionals finds no candidate';
multi sub m($x, :$must!) { 'ok' }
dies-ok { EVAL 'm(1)' }, 'a missing required named argument finds no candidate';
is m(1, :must), 'ok', 'the required named argument supplied';

# An optional and a defaulted positional.
multi sub o($a, $b?)       { "o:" ~ ($b // 'none') }
multi sub p($a, $b = 7)    { "p:$b" }
is o(1), 'o:none', 'an optional positional left out';
is o(1, 2), 'o:2', 'and given';
is p(1), 'p:7', 'a defaulted positional left out';
is p(1, 2), 'p:2', 'and given';

# A where clause keeps the full matcher (its evaluation order).
my $ran = 0;
multi sub w(Int $x where { $ran++; $x > 0 }, :$flag) { "w" }
multi sub w($x) { "plain" }
is w(5, :flag), 'w', 'a where candidate with a named argument';
is w(-5), 'plain', 'a where candidate refused by its value';
is $ran, 2, 'the where clause ran once per call that reached it';

# Method candidates keep working too.
my class C {
    multi method m(Int $x, :$a) { "int" }
    multi method m(Str $x) { "str" }
}
is C.new.m(1, :a), 'int', 'method candidate with a named argument';
is C.new.m('x'), 'str', 'method candidate by type';
