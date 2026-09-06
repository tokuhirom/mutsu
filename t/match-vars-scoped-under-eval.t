use Test;

# `$/` and its capture views are routine-scoped even when the program contains
# an `EVAL` (`t/match-vars-are-routine-scoped.t` pins the scoping itself).
#
# `EVAL` anywhere in a program latches the process-global
# `REFLECTIVE_NAME_ACCESS_SEEN` flag, and the zero-argument compiled fast call
# used to read that flag to decide whether to install its scoped env overlay.
# With the flag latched it installed none -- and a zero-local routine that
# matched internally then ran `reset_capture_env_vars` directly against the
# CALLER's env, whose `$<name>` keys it REMOVES (they must become callee-local
# tombstones). So `inner-match()` deleted the caller's `$<first>` in any program
# containing an EVAL, which is every file that loads the vendored upstream
# `Test` module. The overlay is now installed regardless of the flag.

plan 4;

my $unused = EVAL '1';

sub inner-match() { "zz" ~~ /(z)/; 1 }

"abc" ~~ /(b)(c)/;
inner-match();

"abc" ~~ /$<first>=(b)(c)/;
is ~$<first>, 'b', 'baseline $<first> with an EVAL in the program';
inner-match();
is ~$<first>, 'b', 'a zero-local sub does not delete the caller $<first>';
is ~$/, 'bc', '... nor clobber the caller $/';
is ~$0, 'c', '... nor the caller $0 (the sole positional of that match)';
