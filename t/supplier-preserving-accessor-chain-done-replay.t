use v6;
use Test;

plan 3;

# A Supplier::Preserving reached through an attribute accessor chain
# (`$holder.body.emit(...)`, `$holder.body.done`) must keep its replay
# semantics exactly like a bare-variable call does. Method calls chained off
# an accessor dispatch through a different "done" arm internally than a call
# on a bare variable; that other arm used to unconditionally reset the
# supplier state, wiping the backlog and the terminal before a late tap could
# ever replay them.

class Holder {
    has $.body;
}

{
    my $s = Supplier::Preserving.new;
    my $h = Holder.new(body => $s);
    $h.body.emit(Buf.new(1, 2, 3));
    $h.body.done;

    my $got;
    my $done = 0;
    $s.Supply.tap: -> $b { $got = $b }, done => { $done++ };
    is $got.elems, 3, 'a tap registered after an accessor-chained done still replays the backlog';
    is $done, 1, 'and then sees done';
}

# The full body-blob-style chain: Promise(supply { whenever ... }) coerces a
# late tap on the same supply, and must also see the backlog + done.
{
    my $s = Supplier::Preserving.new;
    my $h = Holder.new(body => $s);
    $h.body.emit(Buf.new(4, 5, 6, 7));
    $h.body.done;

    my $p = Promise(supply {
        my $buf = Buf.new;
        whenever $s.Supply -> $chunk {
            $buf ~= $chunk;
            LAST emit $buf;
        }
    });
    is $p.result.elems, 4,
        'a Promise-coerced whenever registered after an accessor-chained done replays the backlog';
}
