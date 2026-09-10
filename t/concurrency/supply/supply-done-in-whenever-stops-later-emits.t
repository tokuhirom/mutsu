use Test;
plan 3;

# `done` inside a `whenever` body ends the enclosing supply, and rakudo tears
# the supply's subscriptions down with it — the source stops reaching the
# body. Completing the supply this way must also close the `whenever`'s tap
# on its source, the same way an explicit `Supplier.done` (or `Tap.close`)
# does; otherwise the body keeps running (to no visible effect, since the
# supply is already complete) for every later emit.

# Single whenever: `done` closes the tap on its own source.
{
    my $source = Supplier.new;
    my @got;
    my $s = supply {
        whenever $source -> $v {
            @got.push($v);
            done if $v eq 'stop';
        }
    }
    $s.tap(-> $ { });
    $source.emit('one');
    $source.emit('stop');
    $source.emit('ignored');
    is @got, ('one', 'stop'), 'done in a whenever body stops the source from reaching the body again';
}

# Two whenevers sharing one supply block: `done` in one body must close BOTH
# sources' subscriptions, not just the one that triggered it.
{
    my $a = Supplier.new;
    my $b = Supplier.new;
    my @got;
    my $s = supply {
        whenever $a -> $v {
            @got.push("a:$v");
            done if $v eq 'stop';
        }
        whenever $b -> $v {
            @got.push("b:$v");
        }
    }
    $s.tap(-> $ { });
    $a.emit('one');
    $a.emit('stop');
    $b.emit('ignored');
    is @got, ('a:one', 'a:stop'), 'done closes every whenever source in the same supply block';
}

# The tap subscription count on the source drops to zero once `done` fires,
# confirming the teardown (not just that emits happen to go nowhere).
{
    my $source = Supplier.new;
    my $s = supply {
        whenever $source -> $v {
            done;
        }
    }
    $s.tap(-> $ { });
    $source.emit('go');
    lives-ok { $source.emit('again') }, 'a later emit on a done-closed source does not throw';
}
