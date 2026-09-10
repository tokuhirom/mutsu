use Test;

plan 4;

# A `whenever <Promise>` registered from *inside another whenever's body* — so
# after the supply block's own run is over — was silently dropped: by then there
# is no emit buffer to register a subscription marker into, and the non-react
# arm of `run_whenever_with_value` only knew how to handle a Supply source.

{
    my $sup = Supplier.new;
    my $s = supply {
        whenever $sup.Supply -> $v {
            emit "got-$v";
            whenever Promise.in(0.05) { emit "timer-after-$v" }
        }
    }
    my @got;
    $s.tap: { @got.push($_) };
    $sup.emit(1);
    sleep 0.3;
    is @got, ['got-1', 'timer-after-1'],
        'a promise whenever nested in a whenever body runs its body';
}

{
    # The kept value reaches the nested body as its parameter.
    my $sup = Supplier.new;
    my $p = Promise.new;
    my $s = supply {
        whenever $sup.Supply {
            whenever $p -> $v { emit $v * 2 }
        }
    }
    my @got;
    $s.tap: { @got.push($_) };
    $sup.emit('go');
    $p.keep(21);
    sleep 0.2;
    is @got, [42], 'the nested body sees the kept value';
}

{
    # Nesting one level deeper still works.
    my $sup = Supplier.new;
    my $s = supply {
        whenever $sup.Supply {
            whenever Promise.in(0.03) {
                whenever Promise.in(0.03) { emit 'inner' }
            }
        }
    }
    my @got;
    $s.tap: { @got.push($_) };
    $sup.emit(1);
    sleep 0.4;
    is @got, ['inner'], 'a promise whenever nested two levels deep still runs';
}

{
    # A nested whenever on a Supply source keeps working (it always did).
    my $a = Supplier.new;
    my $b = Supplier.new;
    my $s = supply {
        whenever $a.Supply -> $v {
            emit "a$v";
            whenever $b.Supply -> $w { emit "b$w" }
        }
    }
    my @got;
    $s.tap: { @got.push($_) };
    $a.emit(1);
    sleep 0.1;
    $b.emit(2);
    sleep 0.2;
    is @got, ['a1', 'b2'], 'a nested Supply source is unaffected';
}
