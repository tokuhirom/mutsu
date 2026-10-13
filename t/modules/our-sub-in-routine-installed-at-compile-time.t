use Test;

plan 5;

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
