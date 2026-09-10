use Test;

# `Promise(supply { whenever $nested_derived_supply -> $v {...LAST emit
# $joined} })` -- the shape `Cro::MessageWithBody.body-blob` uses over a
# `preserve()`d nested source (a Supplier::Preserving wrapping a raw parser
# supply whose own LAST phaser can die) -- must genuinely BREAK the outer
# Promise, with the original exception type intact, when the nested source
# quits. Found via ADR-0028 Slice 2's Cro-verification pass
# (todo/tickets/http-response-parser-content-length-too-short-not-thrown.md):
# the promise stayed `Planned` forever (an unhandled quit's `Err` was
# silently discarded by the drive loop's detached thread), and once that was
# fixed, `.cause` mis-wrapped the exception in `X::AdHoc` because of a
# name-based "is this an exception" check that cannot see a user class's
# `is Exception` ancestry when the class name is not literally `X::...` or
# `*Exception*`.

plan 3;

class TooShort is Exception {
    method message() { "too short" }
}

sub raw-parser(Supply $raw-blobs, int $content-length --> Supply) {
    supply {
        my int $expected = $content-length;
        whenever $raw-blobs -> $blob {
            emit $blob;
            $expected -= $blob.elems;
            LAST {
                if $expected != 0 {
                    die TooShort.new;
                }
            }
        }
    }
}

sub preserve(Supply:D $s) {
    my $p = Supplier::Preserving.new;
    $s.tap: { $p.emit($_) }, done => -> { $p.done }, quit => { $p.quit($_) };
    $p.Supply
}

# 1+2. A nested-derived-source quit breaks the outer Promise(supply{...})
# coercion (rather than leaving it Planned forever), with the original
# exception TYPE preserved through `.cause` (not re-wrapped as X::AdHoc).
{
    my $supplier = Supplier.new;
    my $body-byte-stream = preserve(raw-parser($supplier.Supply, 1000));

    my $body-blob = Promise(supply {
        my $joined = Buf.new;
        whenever $body-byte-stream -> $blob {
            $joined.append($blob);
            LAST emit $joined;
        }
    });

    start {
        $supplier.emit(Buf.new(1, 2, 3)); # far short of 1000
        $supplier.done;
    };

    await Promise.anyof($body-blob, Promise.in(5));
    is $body-blob.status, Broken,
        "a nested-derived-source quit breaks the outer Promise(supply) instead of leaving it Planned";
    isa-ok $body-blob.cause, TooShort,
        ".cause preserves the original exception type instead of re-wrapping it in X::AdHoc";
}

# 3. The happy path (content-length fully satisfied) still keeps normally.
{
    my $supplier = Supplier.new;
    my $body-byte-stream = preserve(raw-parser($supplier.Supply, 3));

    my $body-blob = Promise(supply {
        my $joined = Buf.new;
        whenever $body-byte-stream -> $blob {
            $joined.append($blob);
            LAST emit $joined;
        }
    });

    start {
        $supplier.emit(Buf.new(1, 2, 3));
        $supplier.done;
    };

    await Promise.anyof($body-blob, Promise.in(5));
    is-deeply $body-blob.result.list, (1, 2, 3),
        "the happy path (content-length satisfied) still keeps normally with the joined bytes";
}

# vim: expandtab shiftwidth=4
