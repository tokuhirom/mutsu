use Test;

plan 4;

is(("def" ~~ rx:P5/()ef/ && $/[0].from), 1, 'P5 empty capture keeps correct from');
is(
    ("abcd" ~~ rx:P5/((a)(b)c)(d)/ && $/[2].from),
    1,
    'P5 nested capture uses real from offset',
);
is(
    ("abh" ~~ rx:P5/^a(bc+|b[eh])g|.h$/ && $0),
    Nil,
    'P5 unmatched capture resets $0 to Nil',
);
my $backspace = "\b";
is(("a\b" ~~ rx:P5/a$backspace/ && $/), "a\b", 'double-quoted \\b interpolates as backspace');
