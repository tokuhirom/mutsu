use Test;

# ADR-0113 slice 3: a frame-lexical `my sub` the routine body also uses as a
# value (`&name`) stays frame-lexical. It is never in the routine registry, so
# `&name` is built from the chunk's lexical table, capturing the environment
# at the point of use.

plan 22;

sub twice(&c, $v) { c(c($v)) }
sub adder($x) { my sub inc($v) { $v + $x }; twice(&inc, 0) }
is adder(3), 6, '&name passed to another routine captures the enclosing lexicals';
is adder(4), 8, '... per call of the enclosing routine';

sub introspect() { my sub s1 { 1 }; (&s1.name, &s1 ~~ Sub, &s1.arity) }
is-deeply introspect(), ('s1', True, 0), 'the code object introspects as the routine';

sub same-object() { my sub s1 { 1 }; &s1.WHICH eq &s1.WHICH }
ok same-object(), 'every &name read in one activation is the same object';

sub make-adder($n) { my sub add($v) { $v + $n }; &add }
my &add5 = make-adder(5);
my &add7 = make-adder(7);
is add5(1), 6, 'an escaping &name keeps its captures';
is add7(1), 8, '... and each activation has its own';
isnt make-adder(1).WHICH, make-adder(1).WHICH, 'each activation yields a distinct object';

sub inner($v) { "outer $v" }
sub shadowing() {
    my sub inner($v) { "lexical $v" }
    my &g = &inner;
    (g(1), inner(2), (1, 2).map(&inner).List)
}
is-deeply shadowing(), ('lexical 1', 'lexical 2', ('lexical 1', 'lexical 2')),
    '&name denotes the inner sub over a same-named outer one';
is inner(3), 'outer 3', 'the outer sub is untouched';
is-deeply (1, 2).map(&inner).List, ('outer 1', 'outer 2'), 'outer &name untouched';

sub pusher($n) { my @acc; my sub push-it($v) { @acc.push($v) }; (^$n).map(&push-it).eager; @acc }
is-deeply pusher(3), [0, 1, 2], '&name given to map writes an enclosing array';

sub bumper() { my $c = 0; my sub bump() { $c++ }; my @l = &bump xx 3; .() for @l; $c }
is bumper(), 3, 'calls through stored copies of &name write the enclosing scalar';

sub sorter() { my sub k($a, $b) { $b <=> $a }; (3, 1, 2).sort(&k).List }
is-deeply sorter(), (3, 2, 1), '&name as a sort comparator';

sub recurse($n) { my sub r($k) { $k <= 0 ?? 0 !! $k + r($k - 1) }; my &x = &r; x($n) }
is recurse(4), 10, 'a routine reached through &name recurses into itself';

sub wrapped() {
    my sub w($v) { $v * 2 }
    my $h = &w.wrap(-> $v { 99 });
    my $r = w(5);
    &w.unwrap($h);
    ($r, w(5))
}
is-deeply wrapped(), (99, 10), 'a bare call honours a wrapper installed through &name';
is-deeply wrapped(), (99, 10), '... on every call';

sub curried() { my sub t(Int $a, Int $b) { $a - $b }; &t.assuming(10)(3) }
is curried(), 7, '&name.assuming';

sub reducer() { my sub cube($v) { $v ** 3 }; [+] (1..3).map(&cube) }
is reducer(), 36, '&name mapped and reduced';

sub typed() { my sub only-int(Int $v) { $v }; my &f = &only-int; f('x') }
throws-like { typed() }, X::TypeCheck::Binding::Parameter, 'the code object keeps its signature checks';

# A scalar of the same name is a different symbol (`$pick` vs `&pick`), as in
# JSON::Fast's `sub EXPORT` (`$from-json-changed` / `&from-json-changed`).
sub chooser(*@opts) {
    my $pick;
    for @opts { when 'on' { $pick := True } }
    my sub pick($v) { "picked $v" }
    $pick ?? &pick !! -> $v { "default $v" }
}
is chooser('on')(1), 'picked 1', 'same-named scalar and &name coexist';
is chooser()(2), 'default 2', '... the scalar still reads as itself';

sub named-arg(:$fmt = 'x') { my sub fmt($v) { "<$v>" }; fmt($fmt) }
is named-arg(:fmt<y>), '<y>', 'a named parameter of the same name as a called inner sub';
