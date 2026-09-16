use Test;
plan 5;

# ".&sub()" (call-as-sub postfix) inside string interpolation, threading the
# chain's current value through as the invocant -- same contract as ".method()"
# just above it, but for a named sub rather than a method. Found by making
# has-word's own test suite run: its assertion descriptions interpolate
# "$needle.&mööse()", and without this the paren-less "." was left as literal
# text while "&mööse()" was re-parsed as a bareword call with NO arguments,
# dying with "Too few positionals passed".
my sub double(Str:D $s) { $s ~ $s }
my $needle = "ab";
is "$needle.&double()", "abab", '.&sub() interpolates, invocant threaded through';
is "$needle.&double() done", "abab done", '.&sub() interpolation with trailing text';

# No parens: stays literal, matching rakudo (and how a paren-less .method also
# stays uninterpolated).
is "$needle.&double", "ab.&double", '.&sub without parens is left as literal text';

# Chained: a paren-less .method segment before the .&sub() call.
is "$needle.uc.&double()", "ABAB", '.&sub() chains after a paren-less .method';

# Arguments besides the invocant are passed through too.
my sub join-with(Str:D $s, Str:D $sep) { $s ~ $sep ~ $s }
is "$needle.&join-with('-')", "ab-ab", '.&sub() forwards explicit arguments';

# vim: expandtab shiftwidth=4
