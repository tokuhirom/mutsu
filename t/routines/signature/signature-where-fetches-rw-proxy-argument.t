use v6;
use Test;

plan 6;

# An `is rw` call's Proxy that is passed to a slurpy parameter is fetched when
# it binds, so a `where` clause on the slurpy (and the body) sees its value.
role Ast { }
class Col does Ast { }
sub prox is rw { Proxy.new(FETCH => -> $ { Col.new }, STORE => -> $, $v { }) }

sub sub-plus(+@ret where .all ~~ Ast) { "ok {@ret.elems}" }
sub sub-star(*@ret where .all ~~ Ast) { "ok {@ret.elems}" }
is sub-plus(prox), 'ok 1', 'sub: +@ret where';
is sub-star(prox), 'ok 1', 'sub: *@ret where';

class H {
    method plus(+@ret where .all ~~ Ast) { "m ok {@ret.elems}" }
    method star(*@ret where .all ~~ Ast) { "m ok {@ret.elems}" }
    method plain(+@ret) { @ret[0].^name }
    multi method choose(+@ret where .all ~~ Ast) { "multi {@ret.elems}" }
    multi method choose(|) { "other" }
}
is H.new.plus(prox), 'm ok 1', 'method: +@ret where';
is H.new.star(prox), 'm ok 1', 'method: *@ret where';
is H.new.plain(prox), 'Col', 'method: body sees the fetched value';
is H.new.choose(prox), 'multi 1', 'multi method candidate selection sees the value';
