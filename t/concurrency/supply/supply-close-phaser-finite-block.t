use Test;

# A `supply { }` block that ends with its body runs its CLOSE phaser (#12873),
# in rakudo's order: after the downstream `done` of a plain end, before the
# `done` / `quit` of a body `done`, a `die` or finished cold `whenever`s.

plan 9;

{
    my @log;
    supply { CLOSE @log.push: 'CLOSE'; emit 1 }.tap(done => { @log.push: 'done' });
    @log.push: 'after';
    is-deeply @log, [<done CLOSE after>], 'a finite body: done, then CLOSE';
}

{
    my @log;
    supply { CLOSE @log.push: 'CLOSE'; emit 1 }.tap;
    @log.push: 'after';
    is-deeply @log, [<CLOSE after>], 'a tap without done => still runs CLOSE';
}

{
    my @log;
    supply { CLOSE @log.push: 'CLOSE'; emit 1; done }.tap(done => { @log.push: 'done' });
    is-deeply @log, [<CLOSE done>], 'a body done: CLOSE, then done';
}

{
    my @log;
    my $t = supply { CLOSE @log.push: 'CLOSE'; emit 1 }.tap;
    $t.close;
    is-deeply @log, [<CLOSE>], 'a later Tap.close does not run CLOSE again';
}

{
    my @log;
    supply { CLOSE @log.push: 'CLOSE'; whenever Supply.from-list(1, 2) { } }.tap(
        done => { @log.push: 'done' });
    is-deeply @log, [<CLOSE done>], 'finished cold whenevers: CLOSE, then done';
}

{
    my @log;
    supply { CLOSE @log.push: 'CLOSE'; die 'x' }.tap(quit => { @log.push: 'quit' });
    is-deeply @log, [<CLOSE quit>], 'a die: CLOSE, then quit';
}

{
    my @log;
    my @got = supply { CLOSE @log.push: 'CLOSE'; emit 7 }.list;
    is-deeply @got, [7], '.list of the block';
    is-deeply @log, [<CLOSE>], '... runs its CLOSE once';
}

{
    my @log;
    react { whenever supply { CLOSE @log.push: 'CLOSE'; emit 8 } { @log.push: "got $_" } }
    is-deeply @log, ['got 8', 'CLOSE'], 'a react whenever over the block runs its CLOSE';
}
