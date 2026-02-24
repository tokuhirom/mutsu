use Test;
plan 9;

ok "a" ~~ rx:P5/a/, "rx:P5 matches";
nok "b" ~~ rx:P5/a/, "rx:P5 mismatch";

ok "foo" ~~ m:P5/(?<meow>.+)/, "m:P5 named capture with (?<>)";
isa-ok $<meow>, Match, "named capture is Match";
is $<meow>, "foo", "named capture value";

ok "foo42BAR" ~~ m:P5/(?<meow>[a-z]+)(\d+)(?'moo'[A-Z]+)/, "mixed named syntaxes and positional";
is $/[0], "42", "positional capture excludes named captures";
is $<meow>, "foo", "first named capture";
is $<moo>, "BAR", "second named capture";
