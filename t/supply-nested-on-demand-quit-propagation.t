use Test;

# A source `quit` propagating through two or more levels of chained on-demand
# `supply { whenever <on-demand supply> { ... } }` blocks must reach the
# outermost tap's `quit =>` handler (ADR-0031 Decision A: a supply block's own
# emitter is what "quit" means for that block, and each level re-derives its
# destination from that emitter rather than from the source it happens to be
# subscribed to). Every expectation below was cross-checked against `raku`
# first.

plan 13;

# --- two levels ------------------------------------------------------------
{
    my $sup = Supplier.new;
    my $src = supply { whenever $sup.Supply -> $v { emit $v } }
    my $out = supply { whenever $src -> $v { emit $v } }
    my $died = False;
    my $reason = '';
    my $done = False;
    my @got;
    $out.tap({ @got.push($_) },
             done => { $done = True },
             quit => { $died = True; $reason = ~$_ });
    $sup.emit('a1');
    $sup.quit("boom");
    sleep 0.2;
    is-deeply @got, ['a1'], 'two-level chain delivered the value before the quit';
    ok $died, 'two-level chain: source quit reached the outer tap quit handler';
    is $reason, 'boom', 'two-level chain: the quit reason survived both hops';
    nok $done, 'two-level chain: a quit does not also fire the done handler';
}

# --- three levels ----------------------------------------------------------
{
    my $sup = Supplier.new;
    my $l1 = supply { whenever $sup.Supply -> $v { emit $v } }
    my $l2 = supply { whenever $l1 -> $v { emit $v } }
    my $l3 = supply { whenever $l2 -> $v { emit $v } }
    my $died = False;
    my $reason = '';
    my @got;
    $l3.tap({ @got.push($_) }, quit => { $died = True; $reason = ~$_ });
    $sup.emit('b1');
    $sup.quit("kaboom");
    sleep 0.2;
    is-deeply @got, ['b1'], 'three-level chain delivered the value before the quit';
    ok $died, 'three-level chain: quit propagation is transitive';
    is $reason, 'kaboom', 'three-level chain: the quit reason survived three hops';
}

# --- a QUIT phaser at the chained level handles it -------------------------
# When the `whenever`'s own QUIT phaser handles the exception, the enclosing
# supply completes with `done` instead of `quit` - the tap's `quit =>` must NOT
# fire. This is the negative pin that keeps the unconditional registration from
# over-firing.
{
    my $sup = Supplier.new;
    my $src = supply { whenever $sup.Supply -> $v { emit $v } }
    my $out = supply {
        whenever $src -> $v {
            emit $v;
            QUIT { default { emit "handled:" ~ .message; True } }
        }
    }
    my $died = False;
    my $done = False;
    my @got;
    $out.tap({ @got.push($_) }, done => { $done = True }, quit => { $died = True });
    $sup.emit('c1');
    $sup.quit("oops");
    sleep 0.2;
    is-deeply @got, ['c1', 'handled:oops'], 'a handling QUIT phaser emitted from the chained level';
    nok $died, 'a handled quit does not reach the outer tap quit handler';
    ok $done, 'a handled quit completes the enclosing supply with done';
}

# --- one level (unchanged behaviour) ---------------------------------------
{
    my $sup = Supplier.new;
    my $src = supply { whenever $sup.Supply -> $v { emit $v } }
    my $died = False;
    my $reason = '';
    my @got;
    $src.tap({ @got.push($_) }, quit => { $died = True; $reason = ~$_ });
    $sup.emit('d1');
    $sup.quit("single");
    sleep 0.2;
    ok $died, 'single-level quit propagation still works';
    is $reason, 'single', 'single-level quit reason unchanged';
}

# --- .list over a two-level chain whose source quits -----------------------
# Before the fix the drain never saw a terminal event, so `.list` returned the
# values it had collected instead of throwing (and, for a source that quits
# without closing its taps, waited out the whole drain deadline).
{
    my $sup = Supplier.new;
    my $src = supply { whenever $sup.Supply -> $v { emit $v } }
    my $out = supply { whenever $src -> $v { emit $v } }
    start { sleep 0.1; $sup.emit('f1'); $sup.emit('f2'); $sup.quit("listboom"); }
    my $err = '';
    try {
        my @got = $out.list;
        CATCH { default { $err = .message } }
    }
    is $err, 'listboom', '.list over a two-level chain throws the source quit';
}
