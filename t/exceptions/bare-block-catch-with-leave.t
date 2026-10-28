use v6;
use Test;

plan 6;

# A bare block carrying both CATCH and LEAVE (or ENTER/KEEP/UNDO) is an
# implicit try region around the phaser scope: the CATCH handles the exception
# and the phasers still run.
my @log;

sub tail-block { { CATCH { default { @log.push("catch") } }; LEAVE @log.push("leave"); die "w" } }
tail-block();
is @log.join(','), 'catch,leave', 'tail bare block of a sub: CATCH handles, LEAVE runs';

@log = ();
{ CATCH { default { @log.push("catch") } }; LEAVE @log.push("leave"); die "w" }
is @log.join(','), 'catch,leave', 'statement-position bare block';

@log = ();
sub with-value { { CATCH { default { @log.push("catch") } }; LEAVE @log.push("leave"); 42 } }
is with-value(), 42, 'a bare block that does not die keeps its value';
is @log.join(','), 'leave', 'and runs only LEAVE';

@log = ();
sub with-phasers {
    { CATCH { default { @log.push("catch " ~ .message) } }; ENTER @log.push("enter"); UNDO @log.push("undo"); die "z" };
    @log.push("after");
}
with-phasers();
is @log.join(','), 'enter,catch z,undo,after', 'ENTER/UNDO with CATCH';

@log = ();
my $c = -> { { CATCH { default { @log.push("catch") } }; LEAVE @log.push("leave"); die "q" } };
$c();
is @log.join(','), 'catch,leave', 'tail bare block of a pointy block';
