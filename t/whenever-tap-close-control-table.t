use Test;

# The control table behind ADR-0053 slice 2: `Tap.close` is ordered against the
# *emit*, not against the pump that delivers it. Every row below was measured in
# rakudo v2026.07 first; mutsu must give the same answer.
#
# The rule that explains every row: a value is delivered iff it was emitted
# before `.close` was called. Delivery being deferred to the react drive loop's
# pump (`say`-ordering shows `A B C got 1 got 2`) does not move that boundary.

plan 12;

# --- A. Supplier source, `do whenever` inside react ------------------------

{
    my $s = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x) };
        $s.emit(1); $s.emit(2);
        whenever Promise.in(0.1) { done }
    }
    is-deeply @got, [1, 2], 'A1 no close: every emit arrives';
}

{
    my $s = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x) };
        $t.close;
        $s.emit(1); $s.emit(2);
        whenever Promise.in(0.1) { done }
    }
    is-deeply @got, [], 'A2 close before any emit: nothing arrives';
}

{
    my $s = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x) };
        $s.emit(1);
        $t.close;
        $s.emit(2);
        whenever Promise.in(0.1) { done }
    }
    is-deeply @got, [1], 'A3 close between two emits: only the first arrives';
}

{
    my $s = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x) };
        $s.emit(1); $s.emit(2);
        $t.close;
        whenever Promise.in(0.1) { done }
    }
    is-deeply @got, [1, 2], 'A4 close after both emits: both still arrive';
}

# --- B. plain `.tap` outside react: the same four rows, already correct ----

{
    my $s = Supplier.new;
    my @got;
    my $t = $s.Supply.tap(-> $x { @got.push($x) });
    $s.emit(1); $s.emit(2);
    is-deeply @got, [1, 2], 'B1 tap, no close';
}

{
    my $s = Supplier.new;
    my @got;
    my $t = $s.Supply.tap(-> $x { @got.push($x) });
    $s.emit(1);
    $t.close;
    $s.emit(2);
    is-deeply @got, [1], 'B3 tap, close between two emits';
}

{
    my $s = Supplier.new;
    my @got;
    my $t = $s.Supply.tap(-> $x { @got.push($x) });
    $s.emit(1); $s.emit(2);
    $t.close;
    is-deeply @got, [1, 2], 'B4 tap, close after both emits';
}

# --- C. close from inside the subscription's own callback -----------------

{
    my $s = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x); $t.close };
        $s.emit(1); $s.emit(2); $s.emit(3);
        whenever Promise.in(0.1) { done }
    }
    is-deeply @got, [1, 2, 3],
        'C1 a self-close does not un-emit the values already queued with it';
}

{
    my @got;
    react {
        my $t = do whenever Supply.interval(0.05) -> $x {
            @got.push($x);
            $t.close if $x == 1;
        };
        whenever Promise.in(0.4) { done }
    }
    is-deeply @got, [0, 1], 'C2 an interval source stops at a self-close';
}

# --- D. a sibling whenever closing this one -------------------------------

{
    my $s = Supplier.new;
    my $u = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x) };
        whenever $u.Supply -> $y { $t.close };
        $s.emit(1);
        $u.emit('close');
        $s.emit(2);
        whenever Promise.in(0.1) { done }
    }
    is-deeply @got, [1, 2],
        'D1 a sibling close is ordered at the `.close` call, not at its queueing';
}

{
    my $s = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x) };
        $s.emit(1); $s.emit(2);
        $t.close;
        whenever Promise.in(0.05) { $s.emit(3) }
        whenever Promise.in(0.3) { done }
    }
    is-deeply @got, [1, 2], 'D2 an emit from a later pump round is dropped';
}

# --- E. a `whenever` registered, fed and closed inside another one's body --

{
    my $s = Supplier.new;
    my $u = Supplier.new;
    my @got;
    react {
        whenever $u.Supply -> $y {
            my $t = do whenever $s.Supply -> $x { @got.push($x) };
            $s.emit(1); $s.emit(2);
            $t.close;
        }
        $u.emit('go');
        whenever Promise.in(0.3) { done }
    }
    is-deeply @got, [1, 2],
        'E1 a nested subscription closed before adoption still delivers its backlog';
}
