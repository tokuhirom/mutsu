use Test;

# From the CSS::Grammar distribution: a `rule` whose body ends in a separated
# quantifier (`<item>+ % ','? `) returns ratcheted. The rule's trailing
# whitespace is a `<.ws>` after the separator, and nothing follows it inside
# the rule, so the quantifier must not be left backtrackable: a caller that
# fails after the rule must not be offered a shorter list (which re-ran the
# rule's action once per shorter list).

plan 4;

my @log;
class Act { method e($/) { @log.push(~$/) } }

grammar Optional {
    regex TOP { <e> 'x' || <e> 'z' }
    rule e { <d>+ % ','? }
    token d { \d+ }
}
grammar Bracketed {
    regex TOP { <e> 'x' || <e> 'z' }
    rule e { <d>+ % [',']? }
    token d { \d+ }
}

for (Optional, Bracketed) -> $g {
    @log = ();
    ok $g.parse('1 2 3 z', :actions(Act.new)), "{$g.^name} parses";
    is @log.elems, 2, "{$g.^name}: the rule's action runs once per alternative of TOP";
}

done-testing;
