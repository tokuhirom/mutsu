use Test;

# A `when`/`default` body with phasers is a block of its own: its LEAVE fires
# however the clause exits (#17513: Red's SQLite migration lock left a
# `BEGIN IMMEDIATE` statement unfinished because the LEAVE that finishes it
# sat in a `when` block and never ran).

plan 9;

my @log;

sub dies-in-when { given 1 { when 1 { LEAVE @log.push: 'leave'; die 'boom' } } }
@log = ();
dies-ok { dies-in-when }, 'the exception still propagates';
is-deeply @log, ['leave'], 'LEAVE of a when body runs when the body dies';

sub value-of-when { given 1 { when 1 { LEAVE @log.push: 'leave'; 5 } } }
@log = ();
is value-of-when(), 5, 'a when body with LEAVE still succeeds with its value';
is-deeply @log, ['leave'], 'LEAVE of a matched when body runs';

sub value-of-default { given 2 { when 1 { 3 }; default { LEAVE @log.push: 'default'; 7 } } }
@log = ();
is value-of-default(), 7, 'a default body with LEAVE yields its value';
is-deeply @log, ['default'], 'LEAVE of a default body runs';

@log = ();
for 1, 2 {
    when 1 { LEAVE @log.push: "leave $_"; @log.push: 'one' }
    @log.push: "other $_";
}
is-deeply @log, ['one', 'leave 1', 'other 2'], 'when in a for loop leaves before the next iteration';

@log = ();
given 1 {
    when 1 { ENTER @log.push: 'enter'; LEAVE @log.push: 'leave'; @log.push: 'body'; proceed }
    default { @log.push: 'default' }
}
is-deeply @log, ['enter', 'body', 'leave', 'default'], 'proceed leaves the when body before the next clause';

@log = ();
given 3 { when 1 { LEAVE @log.push: 'never' } }
is-deeply @log, [], 'a when that does not match runs no phaser';
