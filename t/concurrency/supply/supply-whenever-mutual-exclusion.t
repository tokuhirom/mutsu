use Test;

plan 2;

# Raku guarantees you can only be in one `whenever` block of a given supply
# block at a time. While one `whenever` handler runs -- even while it is blocked
# inside `await` -- a sibling `whenever` fed by a different trigger on another
# thread must wait for it to finish before running.
#
# Here `whenever $trigger1` keeps $p1, then blocks on `await $p2` (holding the
# supply block's turn). The main thread emits into $trigger2 while it is blocked;
# the second handler must NOT run until the first releases its turn. So the
# emits come out strictly ordered: "a bear" (from the first, once $p2 is kept)
# then "the wolf" (from the second).
{
    my $trigger1 = Supplier.new;
    my $trigger2 = Supplier.new;
    my $p1 = Promise.new;
    my $p2 = Promise.new;
    my $p3 = Promise.new;

    my $s = supply {
        whenever $trigger1 -> $value {
            $p1.keep(True);
            await $p2;
            emit "a $value";
        }
        whenever $trigger2 -> $value {
            emit "the $value";
            $p3.keep(True);
        }
    }

    my @collected;
    $s.tap({ @collected.push($_) });

    start { $trigger1.emit('bear'); }
    await $p1;
    start { $trigger2.emit('wolf'); }
    $p2.keep(True);
    await $p3;

    is @collected, ['a bear', 'the wolf'],
        'sibling whenevers of one supply block are mutually exclusive across threads';
}

# A plain single-whenever supply block still delivers every value in order.
{
    my $trigger = Supplier.new;
    my $s = supply {
        whenever $trigger -> $v {
            emit $v * 10;
        }
    }
    my @got;
    $s.tap({ @got.push($_) });
    $trigger.emit($_) for 1..4;
    is @got, [10, 20, 30, 40], 'single-whenever supply block forwards all emits';
}
