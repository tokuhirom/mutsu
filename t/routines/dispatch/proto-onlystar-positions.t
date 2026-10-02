use Test;

# A `{*}` anywhere in a proto body is its dispatch point. It is resolved from
# the dynamic call chain, not from where it is written (#10746): it reaches
# the nearest caller that has a dispatcher -- a proto body dispatches, a
# method, multi or builtin gives Nil, a plain sub or a block is looked
# through, and no dispatcher at all dies. Each expectation was checked
# against rakudo.

plan 30;

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

# A `{*}` in a closure handed to a method is not the proto's dispatch point:
# the method is the nearest routine with a dispatcher, so the `{*}` evaluates
# to Nil. A `{*}` that is itself the argument is evaluated at the call, like
# any argument.

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

# The callers decide, not the text (#10746).

sub star-in-sub { {*} }
proto p-via-sub($) { star-in-sub() }
multi p-via-sub(1) { 42 }
is p-via-sub(1), 42, 'a {*} in a sub the proto body calls dispatches the proto';

proto p-inner-sub($) { sub inner { {*} }; inner() }
multi p-inner-sub(1) { 42 }
is p-inner-sub(1), 42, 'a {*} in a nested sub the body calls dispatches the proto';

sub wrap-sub { star-in-sub() }
proto p-two-subs($) { wrap-sub() + 1 }
multi p-two-subs(1) { 42 }
is p-two-subs(1), 43, '... through any number of plain subs';

# Through `try`, which is transparent: `throws-like` would call the block
# from a routine that has a dispatcher, where the `{*}` is Nil.
try star-in-sub();
is $!.^name ~ ': ' ~ $!.message,
    'X::NoDispatcher: star-in-sub is not in the dynamic scope of a dispatcher',
    'a {*} with no dispatcher in the call chain dies';

try { my $x = {*} }
isa-ok $!, X::NoDispatcher, 'a {*} in the mainline dies too';

proto p-kept-closure($) { my &c = { {*} }; [1].map(&c).eager }
multi p-kept-closure(1) { 42 }
is p-kept-closure(1).gist, '(Nil)', 'a closure kept in a variable and handed to map gives Nil';

proto p-map-sub($) { [1].map({ star-in-sub() }).eager }
multi p-map-sub(1) { 42 }
is p-map-sub(1).gist, '(Nil)', 'a sub called from a map callback gives Nil';

proto p-forced-late($) { [1].map({ {*} }) }
multi p-forced-late(1) { 42 }
is-run-output({ say p-forced-late(1) }, "(Nil)\n", 'a map forced by say after the proto returned gives Nil');

proto p-gather($) { gather { take star-in-sub() } }
multi p-gather(1) { 42 }
is p-gather(1).gist, '(Nil)', 'a {*} reached from a gather body gives Nil';

class StarMethod { method m { {*} }; method via-sub { star-in-sub() } }
proto p-in-method($) { StarMethod.m }
multi p-in-method(1) { 42 }
is p-in-method(1).gist, 'Nil', 'a {*} in a method the body calls gives Nil';

proto p-method-sub($) { StarMethod.via-sub }
multi p-method-sub(1) { 42 }
is p-method-sub(1).gist, 'Nil', 'a sub reached through a method gives Nil';

multi star-multi(1) { {*} }
proto p-in-multi($) { star-multi(1) }
multi p-in-multi(1) { 42 }
is p-in-multi(1).gist, 'Nil', 'a {*} in a multi candidate the body calls gives Nil';

class ProtoMethodSub { proto method m($) { star-in-sub() }; multi method m(1) { 42 } }
is ProtoMethodSub.m(1), 42, 'a sub called from a proto method body dispatches the method';

proto p-inner-proto($) { inner-proto() }
proto inner-proto() { star-in-sub() }
multi inner-proto() { 7 }
multi p-inner-proto(1) { 42 }
is p-inner-proto(1), 7, 'the innermost proto body is the one dispatched';

proto p-spaced($) { { * } }
multi p-spaced(1) { 42 }
is p-spaced(1), '*', '`{ * }` is a block returning *, not the onlystar term';

proto p-bare-star($) { my $y = 1; * }
multi p-bare-star(1) { 42 }
is p-bare-star(1), '*', 'a bare * statement is a Whatever, not the dispatch';

sub is-run-output(&code, $expected, $desc) {
    my $out = '';
    my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush { } }.new;
    code();
    $*OUT = $PROCESS::OUT;
    is $out, $expected, $desc;
}
