use v6;
use Test;
plan 14;

# `$<name>` for a name absent from the *current* match leaked the previous
# match's value (a stale env key `<name>` shadowed the `$/` AT-KEY fallback
# in `OpCode::GetCaptureVar` forever after — see
# src/vm/vm_misc_codevar.rs:exec_get_capture_var_op). Every top-level match
# install site now purges stale numeric/named capture env vars via
# `Interpreter::reset_capture_env_vars`
# (src/runtime/seq_helpers/regex_captures.rs).

# Before any match has ever run.
ok $<x> === Nil, 'named capture var is Nil before any match';
ok $0 === Nil, 'positional capture var is Nil before any match';

"xb" ~~ / $<x>=<[cdx]> "b" /;
is ~$<x>, 'x', 'named capture set by first match';

"bb" ~~ / "b" "b" /;
ok $<x> === Nil, 'name absent from current pattern reads Nil, not stale value';
is $/.hash.elems, 0, '$/.hash is empty after captureless match';

# A FAILED match clears $/ and named captures (measured rakudo 6.d rule).
"xb" ~~ / $<x>=<[cdx]> "b" /;
"zz" ~~ / "q" /;
ok $<x> === Nil, 'failed match leaves no stale named capture';
ok $/ === Nil, 'failed match sets $/ to Nil';

# Positional analog stays correct.
"ab" ~~ / a (b) /;
"cd" ~~ / cd /;
ok $0 === Nil, 'positional capture cleared by captureless match';

# Sibling match paths.
"xb" ~~ / $<x>=<[cdx]> "b" /;
"cd".match(/cd/);
ok $<x> === Nil, '.match clears stale named captures';

"xb" ~~ / $<x>=<[cdx]> "b" /;
my $s = "cd";
$s ~~ s/c/X/;
ok $<x> === Nil, 's/// clears stale named captures';

my token t { cd }
"xb" ~~ / $<x>=<[cdx]> "b" /;
"cd" ~~ &t;
ok $<x> === Nil, 'token smartmatch clears stale named captures';

grammar StaleG { token TOP { cd } }
"xb" ~~ / $<x>=<[cdx]> "b" /;
StaleG.parse("cd");
ok $<x> === Nil, 'Grammar.parse clears stale named captures';

# The ticket's sub-call repro.
sub sm1 { "xb" ~~ / $<x>=<[cdx]> "b" / }
sub sm2 { "bb" ~~ / "b" "b" / }
sm1();
sm2();
ok $<x> === Nil, 'stale named capture cleared across sub-call matches';

# :g list-$/ — raku gives a Failure here, mutsu gives Nil after the fix;
# !.defined is true for both, and the load-bearing part is "not the
# stale Match".
"xb" ~~ / $<x>=<[cdx]> "b" /;
"cd" ~~ m:g/cd/;
ok !$<x>.defined, 'stale named capture not visible after :g match';
