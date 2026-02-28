use Test;

plan 6;

is-approx(4 ** -½, 0.5, "unicode fraction exponent supports unary minus");
is-deeply(*²(4), 16, "superscript WhateverCode call works");
is-deeply(3³, *³(3), "superscript power matches curried WhateverCode");

my $no = "𐌣";
is-deeply(unival($no), 50, "unival handles uncommon Unicode numeric characters");
is-deeply("$no¹²".EVAL, 50**12, "unicode numeric literal with superscript exponent parses in EVAL");
is-deeply("2**$no²".EVAL, 2**50**2, "string interpolation stops variable name before superscript");
