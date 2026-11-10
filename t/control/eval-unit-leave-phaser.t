use Test;

plan 6;

# An EVAL'd unit is a scope of its own: its ENTER/LEAVE phasers run when the
# unit ends, whether or not the EVAL was called from inside a routine.
my @log;
EVAL Q[ENTER @log.push("enter"); LEAVE @log.push("leave"); @log.push("body")];
is-deeply @log, ["enter", "body", "leave"], 'ENTER and LEAVE of a mainline EVAL unit';

my @after;
EVAL Q[LEAVE @after.push("leave"); @after.push("body")];
@after.push("after");
is-deeply @after, ["body", "leave", "after"], 'LEAVE runs before the EVAL returns';

is EVAL(Q[LEAVE @log.push("tail-leave"); 5]), 5, 'the unit value survives a LEAVE';

sub in-routine { my @r; EVAL Q[LEAVE @r.push("leave"); @r.push("body")]; @r }
is-deeply in-routine(), ["body", "leave"], 'the same inside a routine';

my @died;
try EVAL Q[LEAVE @died.push("leave"); die "boom"];
is-deeply @died, ["leave"], 'LEAVE runs when the unit dies';

my @kept;
EVAL Q[KEEP @kept.push("keep"); UNDO @kept.push("undo"); 1];
is-deeply @kept, ["keep"], 'KEEP runs on success, UNDO does not';
