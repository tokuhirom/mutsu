use Test;

# A `{*}` anywhere in a proto body is its dispatch point, since the rewrite
# walks the body through the exhaustive mutable visitor (ADR-10499). Each
# expectation was checked against rakudo.

plan 14;

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

# A `{*}` in a closure that sits in a method call's arguments is not the
# proto's dispatch point: the method the closure is handed to is the nearest
# routine with a dispatcher, so the `{*}` evaluates to Nil. A `{*}` that is
# itself the argument is evaluated at the call, like any argument.

proto p-map($) { [1].map({ {*} }).eager }
multi p-map(1) { 42 }
is p-map(1).gist, '(Nil)', 'a {*} in a map callback is Nil, not the dispatch';

proto p-lazy($) { [1].map({ {*} }) }
multi p-lazy(1) { 42 }
is p-lazy(1).list.gist, '(Nil)', '... also when the Seq is forced after the proto returned';

proto p-first($) { [1].first({ {*} }) }
multi p-first(1) { 42 }
is p-first(1).gist, 'Nil', '... for any builtin method taking a callback';

proto p-nested-cb($) { [[1]].map({ .map({ {*} }).eager }).eager }
multi p-nested-cb(1) { 42 }
is p-nested-cb(1).gist, '((Nil))', '... however deep the closures nest';

class Caller { method run(&b) { b() } }
proto p-method($) { Caller.new.run({ {*} }) }
multi p-method(1) { 42 }
is p-method(1).gist, 'Nil', '... for a user-defined method too';

sub run-sub(&b) { b() }
proto p-sub($) { run-sub({ {*} }) }
multi p-sub(1) { 42 }
is p-sub(1), 42, 'a closure handed to a plain sub still reaches the dispatch';

proto p-map-arg($) { [1].map({*}).eager }
multi p-map-arg(1) { 42 }
dies-ok { p-map-arg(1) }, 'a {*} that is the argument is the dispatch result (and not callable)';

sub is-run-output(&code, $expected, $desc) {
    my $out = '';
    my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush { } }.new;
    code();
    $*OUT = $PROCESS::OUT;
    is $out, $expected, $desc;
}
