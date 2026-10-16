use Test;

# Sibling bare blocks that each declare `my $a` share one frame slot
# (`CompiledCode::multi_scope_slots`). A named sub each block declares over
# its `$a` still captures that block's own variable in a cell, so its writes
# reach the block without any frame's return merge carrying them, and the
# siblings stay independent (ADR-12529 §3, slice 23).

plan 9;

{
    is foo(), 0, 'a sub called before its block\'s `my` runs';
    is foo(), 1, 'keeps its write';
    my $a;
    sub foo { $a++ }
}

{
    is bar(), 0, 'the sibling block\'s sub starts from its own variable';
    is bar(), 1, 'and accumulates in it';
    my $a = 3;
    sub bar { $a++ }
}
my $after = 1;

my &get;
my &inc;
{
    my $a = 10;
    sub baz { $a++ }
    &get = -> { $a };
    &inc = &baz;
}
inc();
inc();
is get(), 12, 'a sub declared after the block\'s `my` writes the block\'s variable';

{
    my @log;
    sub log-one { @log.push: 'one' }
    sub log-two { @log.push: 'two' }
    log-one();
    log-two();
    is @log.join(','), 'one,two', 'two subs over one container share it with the block';
}

{
    my @log;
    sub log-three { @log.push: 'three' }
    log-three();
    is @log.join(','), 'three', 'a sibling block\'s container is its own';
}

{
    my $b = 5;
    sub thr { ++$b }
    await start { thr() };
    is $b, 6, 'a write from another thread reaches the block';
}

{
    my $b;
    is ($b // 'undef'), 'undef', 'a later sibling\'s `my` is a fresh variable';
}
