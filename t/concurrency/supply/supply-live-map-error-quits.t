use Test;

# An exception thrown by the callable of a live `Supply.map` / `Supply.do`
# stage quits that stage's supply: it reaches the tap's `quit` handler (or a
# `whenever`'s QUIT phaser) instead of unwinding into the `Supplier.emit` that
# fed the stage (#18473; Red t/190-resultseq-supply).

plan 9;

{
    my $s = Supplier.new;
    my $err;
    my @got;
    $s.Supply.map(-> $v { die "boom" if $v == 2; $v * 10 }).tap(
        -> $v { @got.push($v) }, quit => -> $e { $err = $e });
    lives-ok { $s.emit(1); $s.emit(2) }, 'emit returns normally when the map dies';
    is $err.message, 'boom', 'the map exception reaches quit';
    is-deeply @got, [10], 'values before the failure were delivered';
    $s.emit(3);
    is-deeply @got, [10], 'the quit stage takes no further values';
}

{
    my $s = Supplier.new;
    my $err;
    $s.Supply.do(-> $v { die "do-boom" }).tap(-> $ { }, quit => -> $e { $err = $e });
    $s.emit(1);
    is $err.message, 'do-boom', 'a dying do callable quits too';
}

{
    my $s = Supplier.new;
    my $err;
    my $quits = 0;
    my $mapped = $s.Supply.map(-> $ { die X::AdHoc.new(:payload<typed>) });
    $mapped.tap(-> $ { }, quit => -> $e { $err = $e; $quits++ });
    $s.emit(1);
    isa-ok $err, X::AdHoc, 'the exception object is passed as is';
    is $quits, 1, 'quit runs once';
}

{
    my $s = Supplier.new;
    my $caught;
    my $p = start react {
        whenever $s.Supply.map(-> $ { die "in-react" }) {
            QUIT { default { $caught = .message } }
        }
        whenever Promise.in(0.05) { $s.emit(1) }
    }
    await Promise.anyof($p, Promise.in(10));
    is $caught, 'in-react', 'a whenever QUIT phaser sees the map exception';
}

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(* + 1).tap(-> $v { @got.push($v) });
    $s.emit(1);
    $s.emit(2);
    is-deeply @got, [2, 3], 'a map that does not die is unaffected';
}
