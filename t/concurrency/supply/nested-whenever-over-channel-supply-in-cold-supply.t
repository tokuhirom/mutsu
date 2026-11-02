use Test;

plan 5;

# A `whenever` registered by the BODY of a `whenever` of a `react` (the drive
# loop adopts it) over a cold `supply { }` whose own body has a
# `whenever <Channel.Supply>` must receive what that channel is fed later. The
# adopted inner subscription used to be dropped (the builder behind it had no
# `Channel.Supply` arm) and replayed as a finite source (#16026).

sub run-nested(&make-source, &feed) {
    my $outer = Supplier.new;
    my $ready = Promise.new;
    my $got   = Promise.new;
    my $inner = Supplier.new;
    start react {
        whenever $outer.Supply -> $ {
            whenever make-source($inner) -> $v { $got.keep($v) unless $got }
        }
        $ready.keep(True);
    }
    await $ready;
    $outer.emit(1);
    sleep 0.2;
    feed($inner);
    await Promise.anyof($got, Promise.in(3));
    $got.status ~~ Kept ?? $got.result !! 'TIMEOUT'
}

is run-nested(-> $s { supply { whenever $s.Supply.Channel.Supply { emit $_ } } }, -> $s { $s.emit('a') }),
    'a', 'a cold supply over Channel.Supply, nested in a whenever body';

is run-nested(-> $s { supply { whenever $s.Supply { emit $_ } } }, -> $s { $s.emit('b') }),
    'b', 'a cold supply over a plain Supply (always worked)';

is run-nested(-> $s { $s.Supply.Channel.Supply }, -> $s { $s.emit('c') }),
    'c', 'Channel.Supply directly (always worked)';

is run-nested(-> $s { supply { whenever $s.Supply.Channel.Supply { emit .uc } } }, -> $s { $s.emit('d') }),
    'D', 'the body of the cold supply transforms what it receives';

# The same at the top level of the react (never failed).
{
    my $s = Supplier.new;
    my $got = Promise.new;
    start react { whenever supply { whenever $s.Supply.Channel.Supply { emit $_ } } -> $v { $got.keep($v) } }
    sleep 0.2;
    $s.emit('e');
    await Promise.anyof($got, Promise.in(3));
    is ($got.status ~~ Kept ?? $got.result !! 'TIMEOUT'), 'e', 'the same shape directly in the react body';
}
