use Test;

# A `whenever` in a `supply` block subscribes when it runs: what its live
# source emits while the block body is still running is delivered (after the
# body), not lost (#18524; Red's ResultSeq.Supply forwards inserts made during
# its snapshot through such a supplier, t/190-resultseq-supply 33/34).

plan 5;

{
    my $s = supply {
        my $forward = Supplier.new;
        whenever $forward.Supply -> $v { emit "fwd:$v" }
        $forward.emit(1);
        emit "body";
        $forward.emit(2);
    };
    my @got;
    $s.tap({ @got.push: $_ });
    is-deeply @got, ['body', 'fwd:1', 'fwd:2'], 'emits during the body arrive after it';
}

{
    # Emitted from a tap callback the body triggers, as Red does.
    my $events = Supplier.new;
    my $s = supply {
        my $forward = Supplier.new;
        whenever $forward.Supply -> $v { emit "live:$v" }
        my $t = $events.Supply.tap(-> $v { $forward.emit($v) });
        emit "snap:$_" for 1..2;
        $events.emit('during');
        emit "snap:3";
    };
    my @got;
    $s.tap({ @got.push: $_ });
    $events.emit('after');
    is-deeply @got, ['snap:1', 'snap:2', 'snap:3', 'live:during', 'live:after'],
        'a value forwarded during the body follows it, later ones stay live';
}

{
    # Nothing is delivered twice once the whenever is subscribed.
    my $src = Supplier.new;
    my $s = supply { whenever $src.Supply -> $v { emit $v } };
    my @got;
    $s.tap({ @got.push: $_ });
    $src.emit($_) for 1..3;
    is-deeply @got, [1, 2, 3], 'live values after setup arrive once';
}

{
    # Each tap of the block subscribes for itself.
    my $s = supply {
        my $forward = Supplier.new;
        whenever $forward.Supply -> $v { emit $v * 10 }
        $forward.emit(4);
    };
    my @a; my @b;
    $s.tap({ @a.push: $_ });
    $s.tap({ @b.push: $_ });
    is-deeply (@a, @b), ([40], [40]), 'per tap';
}

{
    # A Supplier::Preserving source keeps its own backlog handling.
    my $p = Supplier::Preserving.new;
    $p.emit('early');
    my $s = supply { whenever $p.Supply -> $v { emit $v } };
    my @got;
    $s.tap({ @got.push: $_ });
    is-deeply @got, ['early'], 'preserving backlog still replays once';
}
