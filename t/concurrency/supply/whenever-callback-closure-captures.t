use Test;

# A closure made in one invocation of a `whenever` body reads that
# invocation's lexicals, even when a later invocation of the same body calls
# it (#12872, ADR-12529). The body and its LAST/QUIT phasers compile at the
# `whenever` site as ordinary closures, so their closures' free variables are
# their captures, never a same-named lexical of the frame that runs them.
# Every expectation was checked against rakudo.

plan 7;

{
    my @seen;
    my $s = Supplier.new;
    my @cl;
    my $outer = supply {
        whenever $s.Supply -> $x {
            if $x eq 'stop' { .() for @cl }
            else { @cl.push: { @seen.push: "closure sees $x" } }
        }
    }
    $outer.tap;
    $s.emit('a');
    $s.emit('b');
    $s.emit('stop');
    is @seen.join(','), 'closure sees a,closure sees b',
        'a closure made in a supply whenever body keeps its invocation\'s parameter';
}

{
    my @seen;
    my @cl;
    react {
        whenever Supply.from-list(<a b stop>) -> $x {
            if $x eq 'stop' { .() for @cl }
            else { @cl.push: { @seen.push: $x } }
        }
    }
    is @seen.join(','), 'a,b', 'the same in a react whenever body';
}

{
    my @got;
    my $outer = supply {
        whenever Supply.from-list(1, 2) { emit $_ * 10 }
    }
    $outer.tap({ @got.push: $_ });
    is @got.join(','), '10,20', 'a whenever body without a signature binds its topic';
}

{
    my @got;
    react {
        whenever Supply.from-list(1, 2) -> $v {
            @got.push: $v;
            LAST { @got.push: 'last' }
        }
    }
    is @got.join(','), '1,2,last', 'a LAST phaser runs once the source is done';
}

{
    my $caught;
    my $s = Supplier.new;
    start { sleep 0.1; $s.quit(X::AdHoc.new(payload => 'boom')) }
    react {
        whenever $s.Supply {
            QUIT { default { $caught = .message } }
        }
    }
    is $caught, 'boom', 'a QUIT phaser receives the exception as its topic';
}

{
    my $total = 0;
    react {
        whenever Supply.from-list(1, 2, 3) { $total += $_ }
        whenever Supply.from-list(10) { $total += $_ }
    }
    is $total, 16, 'sibling whenever bodies share an outer lexical';
}

{
    my @got;
    my $s = Supplier.new;
    my $t = Supplier.new;
    my $outer = supply {
        whenever $s.Supply -> $a {
            whenever $t.Supply -> $b { emit "$a$b" }
        }
    }
    $outer.tap({ @got.push: $_ });
    $s.emit('x');
    $t.emit(1);
    $s.emit('y');
    $t.emit(2);
    is @got.join(','), 'x1,x2,y2', 'a nested whenever body sees the outer body\'s parameter';
}
