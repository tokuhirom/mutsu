use Test;

plan 4;

throws-like 'my $১০kinds', X::Syntax::Variable::Numeric,
    "Doesn't allow non-ASCII digits at start of identifier";

throws-like 'my $̈a;', X::Syntax::Malformed,
    "Combining marks are not allowed as first character of identifier";

my $ẛ̣ = 42; # LATIN SMALL LETTER LONG S WITH DOT ABOVE + COMBINING DOT BELOW
is $ẛ̣, 42, 'Identifiers are canonically normalized';

my $ﬁ = True;
my $fi = False;
is $fi, False, 'Identifier normalization does not use compatibility decomposition';
