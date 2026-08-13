use Test;

# A `die` escaping a `whenever`'s `LAST` phaser must terminate the enclosing
# supply via `quit` (delivering to a `quit =>` handler / the `.tap()`'s
# `quit` slot), exactly like a `die` inside the whenever's main body already
# does -- NOT complete it via the ordinary `done =>` handler, and not both.
# This is the general mechanism behind Cro::HTTP::RawBodyParser::ContentLength's
# "connection closed before Content-length bytes received" check
# (`LAST { die X::Cro::HTTP::RawBodyParser::ContentLength::TooShort.new if
# $expected != 0 }`), found via ADR-0028 Slice 2's Cro-verification pass
# (todo/tickets/http-response-parser-content-length-too-short-not-thrown.md).

plan 4;

# 1. A die inside LAST reaches the quit => handler with the right exception,
# and the done => handler must NOT also run (a supply terminates via either
# done or quit, never both).
{
    class TooShort is Exception {
        method message() { "too short" }
    }

    my $supplier = Supplier.new;
    my $out = supply {
        whenever $supplier.Supply -> $blob {
            emit $blob;
            LAST {
                die TooShort.new;
            }
        }
    };

    my $caught;
    my $done-ran = False;
    my $done = Promise.new;
    $out.tap(
        -> $v { },
        quit => -> $ex { $caught = $ex; $done.keep(True); },
        done => { $done-ran = True; $done.keep(True); },
    );

    start {
        $supplier.emit(Buf.new(1, 2, 3));
        $supplier.done;
    };

    await Promise.anyof($done, Promise.in(5));
    ok $caught.isa(TooShort), "a die inside LAST reaches the quit => handler with the right exception";
    nok $done-ran, "the done => handler does not also run after LAST converted to quit";
}

# 2. LAST sees the outer variable's latest mutated value (the ContentLength
# check's `$expected -= $blob.elems` shape) and only dies when the condition
# is actually met -- a sibling case that completes cleanly must still just
# call done => normally (no accidental quit on the happy path).
{
    my $supplier = Supplier.new;
    my $out = supply {
        my int $expected = 3;
        whenever $supplier.Supply -> $blob {
            emit $blob;
            $expected -= $blob.elems;
            LAST {
                die "too short: $expected left" if $expected != 0;
            }
        }
    };

    my $caught;
    my $done-ran = False;
    my $done = Promise.new;
    $out.tap(
        -> $v { },
        quit => -> $ex { $caught = $ex; $done.keep(True); },
        done => { $done-ran = True; $done.keep(True); },
    );

    start {
        $supplier.emit(Buf.new(1, 2, 3)); # exactly 3 bytes -- $expected hits 0
        $supplier.done;
    };

    await Promise.anyof($done, Promise.in(5));
    ok $done-ran, "LAST completing without dying still delivers done => normally";
    nok $caught.defined, "no spurious quit on the happy path";
}

# vim: expandtab shiftwidth=4
