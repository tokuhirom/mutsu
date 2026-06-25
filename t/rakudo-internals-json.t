use Test;
plan 4;
my $d = Rakudo::Internals::JSON.from-json(q/{"a":1,"b":[2,3],"c":"x"}/);
is $d<a>, 1, "from-json scalar";
is $d<b>[1], 3, "from-json nested array";
is $d<c>, "x", "from-json string";
my $j = Rakudo::Internals::JSON.to-json({n => 5});
like $j, /\"n\"/, "to-json emits key";
