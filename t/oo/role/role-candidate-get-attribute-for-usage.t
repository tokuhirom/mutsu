use Test;

# A role candidate answers `.^get_attribute_for_usage` and `.^attribute_table`
# for the attributes its body declares (Config::BINDish's
# `Container.^candidates[0].^get_attribute_for_usage('$!payload')`, #14186).

plan 6;

role R { has Mu $.payload is built(:bind) is default(Any); has $!priv; }

my $cand = R.^candidates[0];
is $cand.^get_attribute_for_usage('$!payload').name, '$!payload', 'a public attribute';
is $cand.^get_attribute_for_usage('$!priv').name, '$!priv', 'a private attribute';
is $cand.^attribute_table.keys.sort.join(','), '$!payload,$!priv', 'the attribute table';
throws-like { $cand.^get_attribute_for_usage('$!nope') }, Exception,
    'an attribute the role does not declare';

role P[::T] { has T $.typed; method m { } }
role P { has $.plain }
is P.^candidates.map({ .^attribute_table.keys.join }).sort.join(','), '$!plain,$!typed',
    'each candidate of a role group has its own attributes';
is P.^candidates.map({ .^methods.map(*.name).join }).join('|'), 'm|',
    'each candidate of a role group has its own methods';
