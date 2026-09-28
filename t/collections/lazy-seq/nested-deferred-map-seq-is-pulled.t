use Test;

# ADR-0058 made `.map` return a `Seq` whose callback runs at first consumption.
# A nested `.map` stays deferred while the outer Seq is consumed. Rendering
# or flattening the outer Seq reads the nested elements and reifies them then.
#
# Every expectation was verified by running this file under real `raku`.

plan 13;

my $sink-calls = 0;
<a b>.map({ (1, 2).map({ $sink-calls++ }) });
is $sink-calls, 0, 'sinking the outer map leaves its returned Seqs deferred';

my $calls = 0;
my @stored = <a b>.map({ (1, 2).map({ $calls++ }) });
is @stored.elems, 2, 'array assignment consumes only the outer map';
is $calls, 0, 'inner maps remain deferred in array elements';
is @stored[0].elems, 2, 'reading one inner Seq consumes it';
is $calls, 2, 'the other inner Seq remains deferred';

sub nested { [1].map(-> $e { [2].map(-> $x { "STOP" }) }) }

is nested().raku, '(("STOP",).Seq,).Seq', '.raku renders the inner Seq';
is nested().gist, '((STOP))', '.gist renders it';
is nested().Str, 'STOP', '.Str renders it';
is nested().flat.join('|'), 'STOP', '.flat reaches the inner elements';
is nested().elems, 1, 'the outer Seq still has one element';
is nested()[0].elems, 1, '... whose own Seq has one element';

# The listop spelling of the same nesting, deferred since ADR-0058 step 3.
is (map { map { "STOP" }, [2] }, [1]).raku, '(("STOP",).Seq,).Seq',
    'the listop spelling agrees';

# Three levels deep.
is [1].map(-> $a { [2].map(-> $b { [3].map(-> $c { "END" }) }) }).gist,
    '(((END)))', 'the descent is recursive, not one level';
