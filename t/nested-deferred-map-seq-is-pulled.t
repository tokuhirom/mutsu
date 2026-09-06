use Test;

# ADR-0058 made `.map` return a `Seq` whose callback runs at first consumption.
# When that callback itself returns a `.map` Seq, pulling the outer one left
# the INNER bodies unpulled, sitting in the elements the pull had just
# produced -- and the pure-value readers `reify_map_grep_seq` exists for then
# saw ADR-0034's empty seed one level down. Pulling a deferred map now pulls
# the deferred maps it produced.
#
# Every expectation was verified by running this file under real `raku`.

plan 8;

sub nested { [1].map(-> $e { [2].map(-> $x { "STOP" }) }) }

is nested().raku, '(("STOP",).Seq,).Seq', '.raku renders the inner Seq';
is nested().gist, '((STOP))', '.gist renders it';
is nested().Str, 'STOP', '.Str renders it';
is nested().flat.join('|'), 'STOP', '.flat reaches the inner elements';
is nested().elems, 1, 'the outer Seq still has one element';
is nested()[0].elems, 1, '... whose own Seq has one element';

# The listop spelling of the same nesting. It does not reach this path at all
# yet: `builtin_map` is still eager (ADR-0058 step 3 is blocked -- see
# `todo/deep/deferred-map-callback-runs-in-the-consuming-frames-env.md`), so it
# answers a plain nested List instead of a Seq of Seqs.
todo 'the listop `map` is still eager (ADR-0058 step 3)';
is (map { map { "STOP" }, [2] }, [1]).raku, '(("STOP",).Seq,).Seq',
    'the listop spelling agrees';

# Three levels deep.
is [1].map(-> $a { [2].map(-> $b { [3].map(-> $c { "END" }) }) }).gist,
    '(((END)))', 'the descent is recursive, not one level';
