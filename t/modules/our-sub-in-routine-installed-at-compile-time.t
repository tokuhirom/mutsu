use Test;

plan 7;

# An `our sub` declared in a mainline routine or block is installed in GLOBAL
# at compile time, whether or not the code around it ever runs (#13091). It
# closes over the declaring activation's lexicals: before that runs they are
# undefined, afterwards bound.

sub declares { our sub helper { 'h' } }
ok GLOBAL::<&helper>.defined, 'installed before the routine runs';
is GLOBAL::helper(), 'h', 'callable through GLOBAL';
ok OUR::<&helper>.defined, 'visible through OUR';

sub closes-over($n) { my $v = $n; our sub reads-v { $v } }
is GLOBAL::reads-v().raku, 'Any', 'sees an undefined lexical before its scope runs';
closes-over(3);
is GLOBAL::reads-v(), 3, 'sees the bound lexical once its scope ran';

# An EVAL'd snippet is a compilation unit of its own, and installs its nested
# `our` routines the same way (#13137).
is EVAL(q[sub f { our sub eval-nested { 6 } }; GLOBAL::eval-nested()]), 6,
    'an EVAL installs an our sub nested in its routine before running';
is EVAL(q[package EvalQ { sub g { our sub in-q { 7 } } }; EvalQ::in-q()]), 7,
    'an EVAL installs one nested in a routine of its package';
