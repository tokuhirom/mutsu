use Test;

plan 3;

# A `whenever` whose source is an ON-DEMAND supply must stay LIVE when that
# supply's own whenevers sit on live suppliers. The supply-block tap path used
# to materialize an on-demand source into a value snapshot: zero buffered
# values, so the body never ran and its LAST phaser fired immediately —
# Cro::ConnectionManager's per-connection pipeline emitted '(closed)' before
# any message could flow. The chained-tap arm propagates liveness; a finite
# source still replays synchronously and fires LAST via its done callback.

my $conn-injection = Supplier.new;
my $send = Supplier.new;

sub per-conn-pipeline($in) {
    supply {
        whenever $in -> $msg {
            emit "GOT:$msg";
            LAST emit '(closed)';
        }
    }
}

my @out;
my $service = supply {
    whenever $conn-injection.Supply -> $conn {
        whenever per-conn-pipeline($send.Supply) -> $v {
            emit $v;
        }
    }
};
$service.tap({ @out.push($_) });

$conn-injection.emit('conn-a');
is @out.elems, 0, 'nothing emitted before a message flows (no premature LAST)';

$send.emit('hello');
is-deeply @out, ['GOT:hello'], 'message flowed through the per-connection pipeline';

$send.done;
is @out.tail, '(closed)', 'LAST fires when the live source completes';
