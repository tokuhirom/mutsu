use Test;

# A `{*}` anywhere in a proto body is its dispatch point, since the rewrite
# walks the body through the exhaustive mutable visitor (ADR-10499). Each
# expectation was checked against rakudo.

plan 7;

proto p-say($) { say {*} }
multi p-say(1) { 42 }
is-run-output({ p-say(1) }, "42\n", 'a {*} as a say argument');

proto p-concat($) { "r=" ~ {*} }
multi p-concat(1) { 42 }
is p-concat(1), 'r=42', 'a {*} as an operand';

proto p-given($) { given 1 { when 1 { {*} } } }
multi p-given(1) { 42 }
is p-given(1), 42, 'a {*} in a when body';

proto p-interp($) { "<{ {*} }>" }
multi p-interp(1) { 42 }
is p-interp(1), '<42>', 'a {*} in an interpolated block';

sub what($x) { $x.^name }
proto p-arg($) { what({*}) }
multi p-arg(1) { 42 }
is p-arg(1), 'Int', 'a {*} as a sub call argument is the dispatch result';

proto p-closure($) { my &c = { {*} }; c() }
multi p-closure(1) { 42 }
is p-closure(1), 42, 'a {*} in a closure the body calls';

proto p-nested($) { sub inner { 7 }; inner() + {*} }
multi p-nested(1) { 42 }
is p-nested(1), 49, 'a nested routine does not disturb the body\'s own {*}';

sub is-run-output(&code, $expected, $desc) {
    my $out = '';
    my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush { } }.new;
    code();
    $*OUT = $PROCESS::OUT;
    is $out, $expected, $desc;
}
