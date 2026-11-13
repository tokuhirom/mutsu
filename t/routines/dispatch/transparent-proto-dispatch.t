use Test;

# A `{*}`-only `proto f(|)` dispatches exactly as the implicit proto of a
# multi family: candidate choice, `nextsame`/`callsame`, sigilless aliasing,
# the no-match error, and wrapping all behave the same, call after call.

plan 10;

my proto sub ins(|) {*}
my multi sub ins(@a, $pos, \value) { @a.splice($pos, 0, value); $pos }
my multi sub ins(IterationBuffer:D $b, $pos, \value) { "buffer" }

sub drive(@a, $v) { ins(@a, 0, $v) }
my @a;
drive(@a, $_) for ^200;
is @a.elems, 200, 'the array candidate ran on every call';
is @a[0], 199, 'and inserted the value';

my proto sub alias(|) {*}
my multi sub alias(\x) { x = 42 }
my $var = 1;
alias($var) for ^150;
is $var, 42, 'a sigilless parameter still aliases the caller variable';
my $ro := 5;
dies-ok { alias($ro) }, 'and refuses a read-only binding';

my proto sub chain(|) {*}
my multi sub chain(Int $x) { "int(" ~ callsame() ~ ")" }
my multi sub chain(Cool $x) { "cool" }
is (^120).map({ chain($_) }).unique.List, ('int(cool)',), 'callsame reaches the next candidate';

my proto sub ns(|) {*}
my multi sub ns(Int $x) { "int:" ~ nextsame() }
my multi sub ns(Any $x) { "any" }
is ns(3), 'any', 'nextsame hands the call on';

my proto sub only-int(|) {*}
my multi sub only-int(Int $x) { $x }
my $str = "s";
throws-like { only-int($str) }, X::Multi::NoMatch, 'no candidate matches';
is (^150).map({ only-int($_) }).sum, (^150).sum, 'repeated typed calls';

my proto sub wrapped(|) {*}
my multi sub wrapped(Int $x) { $x * 2 }
&wrapped.wrap(-> |c { callsame() + 1 });
is (^120).map({ wrapped($_) }).sum, (^120).map({ $_ * 2 + 1 }).sum, 'a wrapped proto runs its wrapper';

proto sub gate(Int $, |) {*}
multi sub gate($x) { "any" }
is gate(1), 'any', 'a proto with a real signature dispatches';
