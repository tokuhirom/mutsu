use Test;

# A code object built from a declared routine (`&foo`, a `.candidates` entry, a
# `nextcallee` candidate, an operator reference) dispatches through the routine's
# own compiled body rather than a re-compile of the AST body the declaration
# copied into it (ADR-0019 C6c). These cover the calling surface that switch has
# to keep intact.

plan 23;

sub twice($n) { $n * 2 }
my &f = &twice;
is f(21), 42, 'a code object bound to a lexical is callable';
is &twice(10), 20, 'a code object is callable at its reference site';
is (&twice).(3), 6, 'a code object is callable through .()';

sub add($a, $b = 5) { $a + $b }
is &add.(1), 6, 'an optional parameter default applies through a code object';
is &add.(1, 2), 3, 'an explicit argument overrides the default';
is &add.arity, 1, 'arity reads the routine signature';
is &add.count, 2, 'count reads the routine signature';

sub named(:$x, :$y = 2) { "$x-$y" }
is &named.(x => 9), '9-2', 'named arguments bind through a code object';

sub empty() { 'no args' }
is &empty.(), 'no args', 'an empty signature is callable with no arguments';
dies-ok { &empty.(1) }, 'an empty signature still rejects arguments';

multi mm(Int $x) { "int $x" }
multi mm(Str $s) { "str $s" }
my &m = &mm;
is m(3), 'int 3', 'multi dispatch picks the Int candidate through a code object';
is m('a'), 'str a', 'multi dispatch picks the Str candidate through a code object';
is &mm.candidates.elems, 2, '.candidates exposes both candidates';
# Indexed positionally rather than by declaration order: mutsu's `.candidates`
# order is still incidental (todo/tickets/multi-candidates-declaration-order.md),
# so select the Int candidate by its signature instead of assuming it is first.
my $int-cand = &mm.candidates.first({ .signature.params[0].type ~~ Int });
is $int-cand.(7), 'int 7', 'a .candidates entry is itself callable';

# A nested named sub escaping its declaring scope: the code object's captured
# env is load bearing, so the switch to the routine's compiled body must not
# drop it.
sub outer() {
    my $secret = 42;
    sub inner() { $secret }
    return &inner;
}
is outer()(), 42, 'an escaped nested sub still sees its captured lexical';

sub sq($x) { $x * $x }
is (1, 2, 3).map(&sq).join(','), '1,4,9', 'a code object works as a map block';
is (1, 2, 3, 4).grep(&even).join(','), '2,4', 'a code object works as a grep block';
sub even($x) { $x %% 2 }

sub infix:<joinbar>($a, $b) { "$a|$b" }
my &op = &infix:<joinbar>;
is op(1, 2), '1|2', 'an operator code object is callable';

sub wrapme($x) { $x + 1 }
&wrapme.wrap(-> $x { callsame() * 10 });
is wrapme(1), 20, 'a wrapped routine dispatches through its wrapper';

sub bump($x is rw) { $x++ }
my $v = 5;
my &b = &bump;
b($v);
is $v, 6, 'an is-rw parameter writes back through a code object';

sub early($x) { return 'early' if $x; 'late' }
is &early.(True), 'early', 'an explicit return works through a code object';
is &early.(False), 'late', 'the implicit return works through a code object';

sub counted() { state $n = 0; ++$n }
my &c = &counted;
c(); c();
is c(), 3, 'a state variable is shared by every call through a code object';
