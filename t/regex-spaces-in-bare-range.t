use Test;

# A `**` quantifier bare range may not have whitespace adjacent to the `..`
# (`m ** 1 ..2` / `m ** 1.. 2`): Raku raises X::Syntax::Regex::SpacesInBareRange.

plan 6;

throws-like '/m ** 1 ..2/', X::Syntax::Regex::SpacesInBareRange,
    pre => { m!'/m ** 1 ..'! },
    post => { m!'2/'! },
    'space before .. in bare range';
throws-like '/m ** 1.. 2/', X::Syntax::Regex::SpacesInBareRange,
    'space after .. in bare range';
throws-like '/m ** 10 .. 20/', X::Syntax::Regex::SpacesInBareRange,
    'spaces both sides of ..';

# Valid forms must still parse and match.
ok 'mm' ~~ /m ** 1..2/, 'no-space bare range still works';
ok 'mmm' ~~ /m ** 2/, 'single count still works';
ok 'mm' ~~ /m ** 1..*/, 'open-ended bare range still works';
