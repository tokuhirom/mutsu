use Test;

plan 17;

# A closure that hands a captured scalar to a call may write it back through
# an `is rw` parameter. No name-write op shows that write, so the capture used
# to be a by-value snapshot that a worker thread could not publish: the
# worker's rw binding boxed a cell of its own and every update was lost
# (#12042). The variable must be a shared cell in the creating frame whenever
# an escaping closure passes it to a call -- including when the variable is
# mentioned nowhere else than inside that closure.
#
# Two things keep these blocks honest:
#  * the creating frame never passes the variable to a call itself, and reads
#    it with a plain copy first. `is $v, ...` would hand it to a call and
#    that, on its own, used to promote the variable to a cell (the creating
#    frame's own call arguments were already analysed), hiding the bug;
#  * every block uses names no other block uses, so a lane left behind by an
#    earlier block cannot make a later one pass by accident.

sub bump-rw($p is rw) { $p⚛++ }
sub set-rw($p is rw)  { $p = 42 }
sub app-rw($p is rw)  { $p ~= 'x' }

# The reported shape: an atomicint named only inside the `start` blocks.
{
    my atomicint $shared = 0;
    await (^3).map: { start { bump-rw($shared) } };
    my $seen = $shared;
    is $seen, 3, 'atomicint bumped through an is rw parameter from start blocks';
}

# The same under heavy contention: one lost update shows.
{
    my atomicint $hot = 0;
    await (^8).map: { start { for ^200 { bump-rw($hot) } } };
    my $seen = $hot;
    is $seen, 1600, 'no update is lost under contention';
}

# A single worker, plain (non-atomic) assignment through the parameter.
{
    my $once = 0;
    await start { set-rw($once) };
    my $seen = $once;
    is $seen, 42, 'a plain assignment through an is rw parameter reaches the creator';
}

# The same inside a routine.
{
    sub in-a-routine { my $r = 0; await start { set-rw($r) }; my $seen = $r; $seen }
    is in-a-routine(), 42, 'the same inside a routine';
}

# Sequential workers accumulate into one container.
{
    my $acc = 'a';
    await start { app-rw($acc) };
    await start { app-rw($acc) };
    my $seen = $acc;
    is $seen, 'axx', 'sequential workers see each other\'s writes';
}

# A closure nested in the start block.
{
    my atomicint $nested = 0;
    await (^3).map: { start { my &f = { bump-rw($nested) }; f() } };
    my $seen = $nested;
    is $seen, 3, 'a closure inside the start block passes the capture on';
}

# A method with an is rw parameter.
{
    class RwCounter { method bump($p is rw) { $p⚛++ } }
    my atomicint $via-method = 0;
    my $c = RwCounter.new;
    await (^3).map: { start { $c.bump($via-method) } };
    my $seen = $via-method;
    is $seen, 3, 'an is rw parameter of a method';
}

# A call through a code variable.
{
    my &bumper = sub ($p is rw) { $p⚛++ };
    my atomicint $via-code = 0;
    await (^3).map: { start { bumper($via-code) } };
    my $seen = $via-code;
    is $seen, 3, 'an is rw parameter of a code variable';
}

# Other ways to get a closure onto another thread.
{
    my atomicint $threads = 0;
    my @t = (^4).map: { Thread.start({ for ^50 { bump-rw($threads) } }) };
    .finish for @t;
    my $seen = $threads;
    is $seen, 200, 'Thread.start';
}

{
    my $promised = 0;
    await Promise.start({ set-rw($promised) });
    my $seen = $promised;
    is $seen, 42, 'Promise.start';
}

{
    my $chained = 0;
    await Promise.kept(1).then({ set-rw($chained) });
    my $seen = $chained;
    is $seen, 42, 'Promise.then';
}

# The binding is per declaration: a loop-body variable is fresh each pass.
{
    my @seen;
    for ^3 -> $i {
        my $fresh = $i;
        await start { app-rw($fresh) };
        my $copy = $fresh;
        @seen.push: $copy;
    }
    is @seen, ['0x', '1x', '2x'], 'a loop-body variable is a fresh binding per iteration';
}

# A parameter forwarded into a start block.
{
    sub run-it($n is rw) { await (^3).map: { start { bump-rw($n) } } }
    my atomicint $forwarded = 0;
    run-it($forwarded);
    my $seen = $forwarded;
    is $seen, 3, 'an is rw parameter forwarded into start blocks';
}

# What the promotion must not change: a callee that only reads its argument
# leaves the variable alone, and the value is still what the closure sees.
{
    sub twice($p) { $p * 2 }
    my $read-only = 5;
    my $got = await start { twice($read-only) };
    my $seen = $read-only;
    is $got, 10, 'a read-only call argument reads the captured value';
    is $seen, 5, 'and leaves the variable alone';
}

# A write that lands after the closure was created is not lost either.
{
    my $later = 1;
    my $p = start { set-rw($later) };
    await $p;
    my $seen = $later;
    is $seen, 42, 'a worker started before the variable was read still writes it';
}

# A closure that never leaves the frame keeps working as before.
{
    my $local = 0;
    my &c = { set-rw($local) };
    c();
    my $seen = $local;
    is $seen, 42, 'a closure run in the creating thread';
}
