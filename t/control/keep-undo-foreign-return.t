use Test;

plan 3;

my @log;
sub auto(&b) { KEEP @log.push("keep"); UNDO @log.push("undo"); b() }

sub s1 { auto({ return 57 }); 0 }
is s1(), 57, 'return through a foreign routine yields the value';
is @log.join(","), "undo", 'UNDO runs when a return leaves the routine early';

@log = ();
sub s2 { auto({ 58 }) }
s2();
is @log.join(","), "keep", 'KEEP runs for a normal defined result';
