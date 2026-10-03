use Test;

# While a `supply { }` body runs, only what it emits on its OWN emitter is
# collected for the tap being set up. Emitting on some other `Supplier` from
# inside the body delivers to that supplier's taps alone; it used to be
# collected as well, so the outer tap received the foreign value too.

plan 3;

{
    my $other = Supplier.new;
    my @other;
    $other.Supply.tap({ @other.push($_) });
    my @outer;
    supply { $other.emit(5); emit 1 }.tap({ @outer.push($_) });
    is-deeply @other.List, (5,), 'the other supplier receives its value';
    is-deeply @outer.List, (1,), 'the outer tap receives only what the body emitted';
}

# The same through `Supply.on-demand`, whose producer gets the emitter explicitly.
{
    my $other = Supplier.new;
    my @other;
    $other.Supply.tap({ @other.push($_) });
    my @outer;
    Supply.on-demand(-> $p { $other.emit('x'); $p.emit('y'); $p.done })
        .tap({ @outer.push($_) });
    is-deeply (@other.List, @outer.List), (('x',), ('y',)),
        'an on-demand producer emitting on another supplier';
}
