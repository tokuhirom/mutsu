use Test;

# A defaulted parameter typed by a `subset` is a bind check: the dispatcher
# trial-binds the default, and a default the subset rejects makes the candidate
# a non-match (#14159).
plan 6;

subset BB of Str where 'content'|'border';

class B {
    has $.num = 5;
    has $.str = 'content';
    multi method a { 'plain' }
    multi method a(BB $box = $!str) { "opt:$box" }
    multi method b { 'plain' }
    multi method b(BB $box = $!num) { "opt:$box" }
    multi method c(BB $box = $!num) { "opt:$box" }
    multi method c { 'plain' }
    multi method d { 'plain' }
    multi method d(BB $box = 'border') is rw { "opt:$box" }
}

is B.new.a, 'opt:content', 'a subset-typed default the subset accepts keeps the candidate';
is B.new.b, 'plain', 'a default the subset rejects drops the candidate';
is B.new.c, 'plain', 'declaration order does not matter';
is B.new.d, 'opt:border', 'a literal default the subset accepts, is rw';
is B.new.a('border'), 'opt:border', 'a supplied argument is checked as before';

multi s(BB $b = 'content') { 'opt' }
multi s { 'plain' }
is s(), 'opt', 'sub with an accepted default still prefers the refined candidate';
