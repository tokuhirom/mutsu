use Test;

plan 3;

# `.list` on a supply block whose whenever source is a PROMISE must run the
# body with the promise's result (and nested whenevers registered by that
# body must be processed too). `supply_get_values` recognized only
# Supply-source markers, so the raw `[Promise, body, [], []]` subscription
# marker leaked to the consumer as a value — Cro::CompositeConnector's
# `establish(...).list` returned the marker instead of the pipeline's
# messages.

my @simple = (supply {
    whenever start { 21 } -> $v {
        emit $v * 2;
    }
}).list;
is-deeply @simple, [42], 'promise whenever replays through .list';

my @nested = (supply {
    whenever start { 'mid' } -> $got {
        whenever Supply.from-list('a', 'b') -> $x {
            emit "$got-$x";
        }
    }
}).list;
is-deeply @nested, ['mid-a', 'mid-b'], 'nested whenever registered by a promise body is processed';

my @plain = (supply { emit 1; emit 2 }).list;
is-deeply @plain, [1, 2], 'plain on-demand list unchanged';
