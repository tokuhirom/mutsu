use Test;

plan 3;

my @log;
for 1..1 { KEEP @log.push("k"); UNDO @log.push("u"); print "" }
is @log.join(","), "k", 'loop body ending in print runs KEEP';

@log = ();
for 1..1 { KEEP @log.push("k"); UNDO @log.push("u"); say "" }
is @log.join(","), "k", 'loop body ending in say runs KEEP';

@log = ();
for 1..1 { KEEP @log.push("k"); UNDO @log.push("u"); Nil }
is @log.join(","), "u", 'loop body ending in Nil still runs UNDO';
