use Test;

plan 4;

# `CALLER::LEXICAL::` lists a `sub` the caller declared inside a block as
# `&name` (Memoize: `memoize('a')` finds the caller's `sub a` by name).

sub n(Str $x) { CALLER::LEXICAL::.AT-KEY('&' ~ $x) // 'none' }
proto sub m(|) {*}
multi sub m(Str $n) { CALLER::LEXICAL::.AT-KEY('&' ~ $n) // 'none' }
proto sub q(|) {*}
multi sub q(Str $n) { CALLER::LEXICAL::{'&' ~ $n} // 'none' }

{
    sub a($x) { $x * 2 }
    is n('a').(21), 42, 'a plain sub sees the caller block sub';
    is m('a').(21), 42, 'through a proto';
    is q('a').(21), 42, 'with a subscript';
    is n('nope'), 'none', 'an unknown name is still absent';
}
