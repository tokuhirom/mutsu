use Test;
use lib 't/lib';

# A module's EXPORT can attach a LEAVE phaser to the scope that `use`s it:
# `($*R.find-attach-target('block') // $*R.find-attach-target('compunit'))
# .add-leave-phaser(...)`. That is how FINALIZER finalizes the resources
# registered in a scope when the scope is left. The expected orders below are
# rakudo's (2026.07, RAKUDO_RAKUAST=1).

plan 9;

sub log-of(&body) {
    my @*ATTACH-LOG;
    body();
    @*ATTACH-LOG.List
}

is-deeply log-of({
    { use AttachLeavePhaser 'bare'; @*ATTACH-LOG.push: 'body' }
}), ('body', 'leave bare'), 'a bare block runs the attached phaser when it is left';

is-deeply log-of({
    {
        LEAVE @*ATTACH-LOG.push: 'outer';
        { use AttachLeavePhaser 'tail'; @*ATTACH-LOG.push: 'inner' }
    }
}), ('inner', 'leave tail', 'outer'),
    'a block in tail position of a block with a LEAVE runs its own attached phaser first';

sub in-sub { use AttachLeavePhaser 'sub'; 42 }
is-deeply log-of({ is in-sub(), 42, 'the routine still returns its value' }), ('leave sub',),
    'a routine body is a block to attach to';

is-deeply log-of({
    { use AttachLeavePhaser 'die'; die 'boom'; CATCH { default { } } }
}), ('leave die',), 'the attached phaser runs when the block is left by an exception';

is-deeply log-of({
    for 1..3 { use AttachLeavePhaser 'loop'; next if $_ == 2 }
}), ('leave loop', 'leave loop', 'leave loop'),
    'a loop body runs the attached phaser once per iteration, `next` included';

is-deeply log-of({
    { use AttachLeavePhaser 'a'; { use AttachLeavePhaser 'b' } }
}), ('leave b', 'leave a'), 'nested blocks each run their own attached phaser';

# `'block'` is Nil at a compunit's top level, so the fallback attaches to the
# compunit: an EVAL is a compunit of its own.
is-deeply log-of({
    EVAL q:to/CODE/;
        use lib 't/lib';
        use AttachLeavePhaser 'eval';
        @*ATTACH-LOG.push: 'eval body';
        CODE
    @*ATTACH-LOG.push: 'after eval';
}), ('eval body', 'leave eval', 'after eval'),
    'a top-level `use` attaches to its compunit, which an EVAL is';

is-deeply log-of({ 1 }), (), 'nothing runs without a `use`';
