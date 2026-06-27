use Test;

# A non-suppressing named-subrule alias `<name=rule>` installs the capture under
# BOTH the alias name AND the subrule's own name (Rakudo behaviour). The dot/amp
# forms (`<name=.rule>` / `<name=&rule>`) suppress the rule-name capture.
# Repeated captures under the same rule name aggregate into a list.
# Regression: roast/integration/advent2013-day06.t (`<offset=count>` + `<count>`).

plan 8;

grammar G {
    token TOP { <x=num> }
    token num { \d+ }
}
my $m = G.parse('5');
ok $m<x>.defined,    '<x=num> creates the alias capture $<x>';
ok $m<num>.defined,  '<x=num> ALSO creates the rule-name capture $<num>';
is ~$m<x>,   '5', 'alias capture value';
is ~$m<num>, '5', 'rule-name capture value';

# dot-alias suppresses the rule-name capture
grammar D {
    token TOP { <x=.num> }
    token num { \d+ }
}
my $d = D.parse('7');
ok $d<x>.defined,     '<x=.num> creates $<x>';
nok $d<num>.defined,  '<x=.num> does NOT create $<num>';

# direct + aliased reference to the same rule aggregate under the rule name
grammar L {
    token TOP { <count> '-' <offset=count> }
    token count { \d+ }
}
my $l = L.parse('3-2');
is $l<count>.elems, 2, 'direct + aliased rule references aggregate into a list';
is ~$l<offset>, '2', 'alias capture is the second reference';
