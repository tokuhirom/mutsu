use Test;

# #9610: a regex literal is a closure, so `$_` inside it is the `$_` of the
# scope it was written in. When such a literal escapes (`.map({ rx{ <$_> } })`),
# matching it must interpolate / expose that captured topic, not the subject.
# mutsu used the subject, so every regex built this way matched everything.

plan 10;

my @r = <ab cd>.map: { rx{ <$_> } };
ok "xab" ~~ @r[0], 'the first regex matches its own word';
nok "xab" ~~ @r[1], 'the second regex does not match the first word';
ok "xcd" ~~ @r[1], 'the second regex matches its own word';
ok "zcd".match(@r[1]), '.match uses the captured topic';
is "xab xcd".subst(@r[1], "Q"), 'xab xQ', '.subst uses the captured topic';
is-deeply <ab cd ef>.grep(@r[1]), ('cd',), '.grep uses the captured topic';

my @seen;
my $r = <ab>.map({ rx{ { @seen.push: $_ } <$_> } })[0];
nok "xcd" ~~ $r, 'a regex with an embedded block still matches by its topic';
is @seen.unique, ('ab',), 'the embedded block sees the captured topic';

my $t;
ok "abc" ~~ / b { $t = $_ } /, 'a non-escaping literal matches';
is $t, 'abc', 'its embedded block sees the subject as $_';
