use v6;
use Test;

plan 8;

# A `LAST done;` phaser inside a `whenever` of a tapped `supply` block
# completes the enclosing supply exactly once, without escaping as an error.
#
# The desugar rewrites the phaser's `done` to `$emitter.done()` followed by a
# SupplyBodyDone control signal. The done-callback dispatch path
# (`invoke_done_callback`) used a bare sub call, so the signal escaped through
# `invoke_done_callback_or_quit` and aborted the producer's own `.done()` call
# site with an empty runtime error; after absorbing it, the source's remaining
# done-chain (the whenever done-group marker) still had to be stopped, or the
# downstream `done =>` handler fired twice. This is the shape
# Cro::HTTP2::GeneralParser uses (`whenever $in { ...; LAST done; }`).

# Case 1: single whenever, LAST done — tap sees the value, done fires once,
# and the producer's `.done` call does not blow up the mainline.
{
    my $in = Supplier.new;
    my $out = supply {
        whenever $in.Supply -> $v {
            emit $v;
            LAST done;
        }
    };
    my @got;
    my $done-count = 0;
    $out.tap: -> $m { @got.push($m) }, done => { $done-count++ };
    $in.emit(3);
    $in.done;
    sleep 0.2;
    is @got.join(','), '3', 'value reached the tap before LAST done';
    is $done-count, 1, 'downstream done fired exactly once';
}

# Case 2: LAST done with a sibling whenever still open forces the supply to
# complete anyway (the HTTP/2 GeneralParser shape: the frame source finishing
# ends the parser even though connection-state whenevers never finish).
{
    my $in = Supplier.new;
    my $other = Supplier.new;
    my $out = supply {
        whenever $other.Supply { emit "other: $_" }
        whenever $in.Supply -> $v {
            emit "in: $v";
            LAST done;
        }
    };
    my @got;
    my $done-fired = False;
    $out.tap: -> $m { @got.push($m) }, done => { $done-fired = True };
    $in.emit(1);
    $in.done;
    sleep 0.2;
    is @got.join(','), 'in: 1', 'only the finished source emitted';
    ok $done-fired, 'LAST done completed the supply despite the open sibling whenever';
}

# Case 3: mainline continues normally after the producer's `.done` (the bug
# aborted the whole process here with an empty runtime error).
{
    my $in = Supplier.new;
    my $out = supply {
        whenever $in.Supply -> $v { emit $v; LAST done; }
    };
    $out.tap: -> $m { };
    $in.emit(42);
    $in.done;
    pass 'mainline survived the producer .done that ran LAST done';
}

# Case 4: `LAST emit ...` (no done) keeps working — the Promise coercion
# resolves with the joined value (the Cro::MessageWithBody.body-blob shape).
{
    my $s = Supplier::Preserving.new;
    my $p = Promise(supply {
        my $joined = '';
        whenever $s.Supply -> $v {
            $joined ~= $v;
            LAST emit $joined;
        }
    });
    $s.emit('a');
    $s.emit('b');
    $s.done;
    await Promise.anyof($p, Promise.in(5));
    is $p.status, Kept, 'LAST emit promise kept';
    is $p.result, 'ab', 'LAST emit still resolves the promise with the joined value';
}

# Case 5: a die in a LAST phaser still routes to the quit handler (must not be
# swallowed by the done-completion handling).
{
    my $in = Supplier.new;
    my $out = supply {
        whenever $in.Supply -> $v {
            emit $v;
            LAST die "boom";
        }
    };
    my $quit = '';
    $out.tap: -> $m { }, quit => { $quit = ~$_ };
    $in.emit(1);
    $in.done;
    sleep 0.2;
    is $quit.substr(0, 4), 'boom', 'die in LAST still reaches the quit handler';
}
