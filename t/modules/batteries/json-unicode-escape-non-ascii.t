use Test;

# A \u escape whose four bytes include a multi-byte character must raise a
# normal parse error, not panic on a char boundary.
plan 4;

dies-ok { Rakudo::Internals::JSON.from-json(q{"\u000é"}) },
    '\u escape containing a non-ASCII character is rejected';
dies-ok { Rakudo::Internals::JSON.from-json(q{"\u00é0"}) },
    'non-ASCII character in the middle of a \u escape is rejected';
dies-ok { Rakudo::Internals::JSON.from-json(q{"\u+123"}) },
    'a sign is not a hex digit in a \u escape';
is Rakudo::Internals::JSON.from-json(q{"é"}), 'é',
    'a valid \u escape still decodes';

done-testing;
