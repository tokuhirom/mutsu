use Test;

plan 3;

# A *%options slurpy hash parameter must always resolve to THIS call's
# arguments, never another thread's live same-named binding via the
# cross-thread shared-variable store (the store is keyed by bare name).
{
    sub read-options(*%options) {
        await start { 0 };
        %options<tag> // 'MISSING';
    }
    sub outer(%h) {
        read-options(|%h);
    }

    my @results;
    my @threads;
    for ^20 -> $i {
        @threads.push: Thread.start({
            for ^5 {
                my %h = tag => "t$i";
                @results.push: (outer(%h) eq "t$i");
            }
        });
    }
    .join for @threads;
    ok all(@results), 'slurpy hash param resolves to its own call across many threads';
}

# A *@items slurpy array parameter must be equally protected.
{
    sub read-items(*@items) {
        await start { 0 };
        @items[0] // 'MISSING';
    }
    sub outer2(@a) {
        read-items(|@a);
    }

    my @results;
    my @threads;
    for ^20 -> $i {
        @threads.push: Thread.start({
            for ^5 {
                my @a = ("t$i",);
                @results.push: (outer2(@a) eq "t$i");
            }
        });
    }
    .join for @threads;
    ok all(@results), 'slurpy array param resolves to its own call across many threads';
}

# A PLAIN (non-slurpy) @/%-sigil parameter must NOT be masked from the
# shared store: a nested nested `start` reading it through the fallback
# name lane must still see the outer call's own binding (regression guard
# for the fix above — masking every @/% parameter unconditionally broke
# this, mirrored by roast/S17-channel/stress.t's bogosort_concurrent).
{
    sub pick-concurrent(@list) {
        my $found = Channel.new;
        start until $found.closed {
            start {
                my @guess = @list.pick(*);
                $found.send(@guess) if @guess.elems == @list.elems;
            }
        }
        return $found.receive;
    }

    my @data = <p e r l>;
    my @ok;
    for ^4 {
        @ok.push: (pick-concurrent(@data).sort.join(',') eq 'e,l,p,r');
    }
    ok all(@ok), 'a plain (non-slurpy) array param is still visible to a nested spawn';
}

done-testing;
