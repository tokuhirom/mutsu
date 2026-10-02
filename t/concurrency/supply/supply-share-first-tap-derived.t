use Test;

# A `.share`d `supply { whenever ... }` block starts on its first tap, however
# that tap arrives: directly, through a derived supply (grep/map/head), or from
# a `whenever` inside another supply block or a react (issue #10740).

plan 11;

{
    my $s = Supplier.new;
    my $sup = supply { whenever $s.Supply -> $d { emit $d } }.share;
    my $c = $sup.grep(* eq 'x').head(1).Promise;
    $s.emit('y');
    $s.emit('x');
    is $c.status, Kept, 'grep.head.Promise on an untapped shared supply is kept';
    is $c.result, 'x', '... with the matching value';
}

{
    my $s = Supplier.new;
    my $sup = supply { whenever $s.Supply -> $d { emit $d } }.share;
    my @got;
    $sup.map(* * 2).tap({ @got.push($_) });
    $s.emit(1);
    $s.emit(2);
    is-deeply @got, [2, 4], 'map on an untapped shared supply sees the values';
}

{
    my $s = Supplier.new;
    my $runs = 0;
    my $sup = supply { $runs++; whenever $s.Supply -> $d { emit $d } }.share;
    my (@a, @b);
    $sup.grep(* > 1).tap({ @a.push($_) });
    $sup.tap({ @b.push($_) });
    $s.emit(1);
    $s.emit(2);
    is $runs, 1, 'the shared block runs once across a derived and a direct tap';
    is-deeply @a, [2], 'the derived tap sees the filtered values';
    is-deeply @b, [1, 2], 'the later direct tap joins the running block';
}

{
    my $s = Supplier.new;
    my $sup = supply { whenever $s.Supply -> $x { emit $x } }.share;
    my @b;
    my $d = supply { whenever $sup -> $x { emit $x } };
    $d.tap({ @b.push($_) });
    $s.emit('y');
    is-deeply @b, ['y'], 'a whenever inside another supply block starts the shared block';
}

{
    my $s = Supplier.new;
    my $runs = 0;
    my $sup = supply { $runs++; whenever $s.Supply -> $x { emit $x } }.share;
    my (@a, @b);
    supply { whenever $sup -> $x { emit $x } }.tap({ @a.push($_) });
    supply { whenever $sup -> $x { emit $x * 10 } }.tap({ @b.push($_) });
    $s.emit(1);
    is $runs, 1, 'two nested whenevers share one run of the block';
    is-deeply [@a, @b], [[1], [10]], '... and both see the value';
}

{
    my $s = Supplier.new;
    my $sup = supply { whenever $s.Supply -> $x { emit $x } }.share;
    my @got;
    my $p = start {
        react {
            whenever $sup -> $x {
                @got.push($x);
                done if $x eq 'stop';
            }
        }
    };
    sleep 0.2;
    $s.emit('a');
    $s.emit('stop');
    await Promise.anyof($p, Promise.in(5));
    is-deeply @got, ['a', 'stop'], 'a react whenever starts the shared block';
}

{
    # Stomp::Client's shape: the shared supply lives in an attribute and is
    # filtered by a WhateverCode inside a method. Checking that closure for an
    # `@_` read used to format its body with `{:?}`, which recursed through
    # the cyclic object graph until the stack overflowed.
    class Conn {
        has $!incoming;
        method connect($source) {
            $!incoming = supply { whenever $source -> $d { emit $d } }.share;
            $!incoming.grep(* eq 'CONNECTED').head(1).Promise
        }
    }
    my $s = Supplier.new;
    my $connected = Conn.new.connect($s.Supply);
    $s.emit('CONNECTED');
    is $connected.status, Kept, 'a shared supply held in an attribute can be filtered';
}
