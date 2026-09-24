use Test;

plan 1;

grammar ProtoRuleMarker {
    proto rule empty {*}
    token TOP { <.empty>? }
}

ok ProtoRuleMarker.parse(''),
    'a candidate-less proto rule is a valid silent subrule';
