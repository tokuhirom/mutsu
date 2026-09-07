use Test;

# A deferred `.map` callback (ADR-0058) runs when the Seq is CONSUMED, which is
# routinely a different frame than the one that called `.map`. Its free
# variables are lexical, so they must resolve to the binding at the `.map` call
# site — never to a same-named lexical that happens to be live in the consuming
# frame. Only `$`-scalars got this right (they are boxed into a shared cell);
# an `@`/`%` container free variable lost to the consumer's.

plan 11;

{
    my @sizes = 1, 2, 3;
    my $s = [1].map({ @sizes.elems });
    sub consumer-with-own-sizes() { my @sizes = 1; $s.List }
    is consumer-with-own-sizes(), (3,), 'file-scope @ free var beats a consumer lexical';
}

{
    sub mk(@p) { [1].map({ @p.elems }) }
    sub consume($q) { my @p = 1; $q.List }
    is consume(mk((7, 8, 9))), (3,), 'a routine @ PARAMETER free var beats a consumer lexical';
}

{
    sub mk2(@p) { [1].map({ @p.elems }) }
    sub consume2($q, @p) { $q.List }
    is consume2(mk2((7, 8, 9)), (1,)), (3,), '... and beats a consumer PARAMETER of the same name';
}

{
    sub mk3(%h) { [1].map({ %h<k> }) }
    sub consume3($q) { my %h = k => 'inner'; $q.List }
    is consume3(mk3({ k => 'outer' })), ('outer',), 'a %-container free var beats a consumer lexical';
}

{
    sub mk4($v) { [1].map({ $v }) }
    sub consume4($q, $v) { $q.List }
    is consume4(mk4('outer'), 'inner'), ('outer',), 'a $-scalar free var still beats a consumer lexical';
}

# The recursive shape this was found through: the callback re-invokes its own
# producer, so the frame active at the pull IS an outer invocation of the same
# routine. Reading `@sizes` from there made `@sizes[1..*]` perpetually `(1,)`
# and the recursion never reached its `@sizes == 0` base case.
{
    my $depth = 0;
    sub recurse(@sizes) {
        $depth++;
        die "runaway recursion" if $depth > 8;
        return "STOP" if @sizes == 0;
        [1].map(-> $e { recurse(@sizes[1..*]).map(-> $x { $x }) })
    }
    my $out = recurse((2, 1)).raku;
    is $depth, 3, 'a recursive deferred-map producer reaches its base case';
    is $out, '(($(("STOP",).Seq),).Seq,).Seq', '... and renders as rakudo does';
}

# Controls: a mutation made after the capture must still be seen, so the
# capture must not become a stale by-value snapshot.
{
    my $a = 1;
    my &b = { $a };
    $a = 5;
    is (1,).map(&b).List, (5,), 'a lexical mutated after closure creation is seen';
}

{
    my $a = 1;
    my $s = (1,).map({ $a });
    $a = 5;
    is $s.List, (5,), 'a lexical mutated after the `.map` call is seen at the pull';
}

# A NESTED deferred map must not leave its own capture behind for the enclosing
# map's next iteration. The capture merge overwrites a same-named key now, so
# every key it overwrites has to be saved and restored around the loop -- not
# only the keys it introduces. Without that, `inner`'s second outer iteration
# read `@sizes` as the recursive call's `(2,)`.
{
    sub inner(@sizes) {
        return $["END"] if @sizes == 0;
        map -> $e {
            map -> $g { "$e/$g" }, inner(@sizes[1..*])
        }, ['a', 'b']
    }
    is inner((1, 2)).map({ .List.raku }).join(' ; '),
        '("a/a/END", "a/b/END") ; ("b/a/END", "b/b/END")',
        'a nested deferred map does not clobber the enclosing one\'s capture';
}

# The same shape one level deeper, with the recursion feeding the inner source.
{
    sub deep(@sizes) {
        return $['X'] if @sizes == 0;
        map -> $e {
            map -> $g { "$e$g" }, deep(@sizes[1..*])
        }, ['p', 'q']
    }
    is deep((1, 2, 3)).map({ .List.map({ .List.raku }).join(',') }).join(' ; '),
        '("pppX pqX",),("pqpX qqX",) ; ("qppX pqX",),("qqpX qqX",)',
        '... at three levels of recursion too';
}
