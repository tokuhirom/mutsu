use Test;

plan 6;

# A Regex object interpolated with `<$re>` keeps its own adverbs: the enclosing
# literal's `:i` does not reach into it, while the object's own `:i` is kept.
my $plain = regex { a+ };
my $icase = rx:i{a+};

ok "a" ~~ m:i{<$plain>}, 'plain object, matching case';
nok "A" ~~ m:i{<$plain>}, 'plain object does not inherit the outer :i';
nok "A" ~~ m:i{<$plain>$}, 'also with an anchor after it';
nok "A" ~~ m:i{^<$plain>}, 'and with an anchor before it';
ok "A" ~~ m{<$icase>}, "an object's own :i is kept";
ok "A" ~~ m:i{<$icase>}, "and still applies under an outer :i";
