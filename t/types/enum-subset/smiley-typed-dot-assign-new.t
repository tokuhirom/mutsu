use Test;
use MONKEY-SEE-NO-EVAL;

plan 7;

# A `.= new` initializer constructs the declared type itself: the definiteness
# smiley qualifies the variable's constraint, not the type that is constructed
# (roast MISC/misc.t, #16437).
is-deeply (my Int:D \b .= new: 42), 42, 'sigilless variable, :D';
is-deeply (my Int:D constant \a .= new: 42), 42, 'sigilless constant, :D';
is-deeply (my Int:D constant $  .= new: 42), 42, 'anonymous scalar constant, :D';
is-deeply (my Int:D $  .= new: 42), 42, 'anonymous scalar variable, :D';
is-deeply (my Int constant \c .= new: 7), 7, 'plain type constant';
is-deeply (my Int \d .= new: 8), 8, 'plain type sigilless variable';

# :U is a constraint too: the constructed Int is defined, so the bind fails.
throws-like { EVAL 'my Int:U \e .= new: 42' }, Exception, ':U sigilless .= new fails the constraint';
