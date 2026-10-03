use Test;
use lib 't/lib';
use ScriptMultiCollision;

# A main-script `multi` and a loaded module's unexported `multi` of the same
# name are two lexical families (#11081): neither joins the other's
# dispatch. Checked against Rakudo.

plan 7;

multi sub smc-family(Str $s) { "script-str:$s" }

is smc-call(1), 'mod-int:1', "the module dispatches to its own candidate";
throws-like { smc-call('x') }, X::Multi::NoMatch,
    "the script's candidate does not join the module's family",
    message => /'(Int $x)'/ & none(/'Str $s'/);
is smc-count(), 1, "&name inside the module sees only its own family";
throws-like { smc-block()('y') }, X::Multi::NoMatch,
    "nor does a block the module returns";

is smc-family('z'), 'script-str:z', 'the script dispatches to its own candidate';
throws-like { my $n = 2; smc-family($n) }, X::Multi::NoMatch,
    "the module's unexported candidate does not join the script's family";
is &smc-family.candidates.elems, 1, '&name in the script sees only its own family';
