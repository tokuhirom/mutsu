use Test;

# A bare `emit`/`done` written inside a *sub* declared in a `supply { }` body is
# not rewritten to `$emitter.emit(...)` by the parser, so it resolves through the
# dynamic emitter stack at run time. When an inner supply's `whenever` fires while
# an outer supply's body is still on the stack, that stack must still name the
# inner supply -- Cro's chunked raw body parser is exactly this shape and used to
# emit its decoded Buf out of the enclosing ResponseParser's supply instead.

plan 6;

sub doubler(Supply $raw) {
    supply {
        whenever $raw -> $v { twice($v) }
        sub twice($x) { emit $x * 2 }
    }
}

# `Cro::HTTP::ResponseParser`'s helper: taps the inner supply from inside another
# sub, one frame deeper than the `whenever` body that created it.
sub preserve(Supply:D $s) {
    my $p = Supplier::Preserving.new;
    $s.tap: { $p.emit($_) }, done => -> { $p.done }, quit => { $p.quit($_) };
    $p.Supply
}

sub run-through(&subscribe) {
    my $wire = Supplier.new;
    my @outer;
    my @inner;
    my $outer = supply {
        whenever $wire -> $v {
            my $raw = Supplier.new;
            subscribe(doubler($raw.Supply), @inner);
            $raw.emit($v);
            emit "marker-$v";
        }
    };
    $outer.tap(-> $x { @outer.push($x) });
    $wire.emit(1);
    $wire.emit(2);
    [@outer.List, @inner.List]
}

for (
    'direct pointy tap' => -> $s, @got { $s.tap(-> $x { @got.push($x) }) },
    'bare block tap'    => -> $s, @got { $s.tap({ @got.push($_) }) },
    'tap with done'     => -> $s, @got { $s.tap(-> $x { @got.push($x) }, done => -> { }) },
    'preserving relay'  => -> $s, @got { preserve($s).tap(-> $x { @got.push($x) }) },
) -> $case {
    my $got = run-through($case.value);
    is-deeply ($got[0], $got[1]),
        (("marker-1", "marker-2"), (2, 4)),
        "nested-sub emit stays in its own supply ({$case.key})";
}

# A bare `done` in a nested sub completes the supply it was written in, and only
# that one: the outer supply keeps running.
{
    sub emit-then-done(Supply $raw) {
        supply {
            whenever $raw -> $v { chunk($v) }
            sub chunk($x) {
                emit $x * 2;
                done;
            }
        }
    }

    my $wire = Supplier.new;
    my @outer;
    my @inner;
    my $outer = supply {
        whenever $wire -> $v {
            my $raw = Supplier.new;
            emit-then-done($raw.Supply).tap(-> $x { @inner.push($x) });
            $raw.emit($v);
            emit "marker-$v";
        }
    };
    $outer.tap(-> $x { @outer.push($x) });
    $wire.emit(1);
    $wire.emit(2);
    is-deeply @outer.List, ("marker-1", "marker-2"),
        'nested-sub done does not terminate the enclosing supply';
    is-deeply @inner.List, (2, 4), 'nested-sub done still delivers its own emit';
}
