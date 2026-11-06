use Test;

plan 13;

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

# The bare `$re` form keeps them too, next to an anchor or not.
my $a = rx{a};
my $A = rx{A};
nok "A" ~~ m:i{^$a$}, 'bare object between anchors does not inherit :i';
nok "A" ~~ m:i{^$a}, 'bare object after ^';
nok "A" ~~ m:i{$a$}, 'bare object before $';
nok "A" ~~ m:i{$a}, 'bare object alone';
ok "A" ~~ m:i{^$A$}, 'exact-case literal inside the object still matches';
nok "a" ~~ m:i{^$A$}, 'and is not folded';
ok "a" ~~ m:i{^$a$}, 'matching case';
