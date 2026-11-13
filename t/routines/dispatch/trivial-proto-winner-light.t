use Test;

# The winner of a `{*}`-only proto binds through the positional light bind
# when it qualifies; sigilless aliasing, nextsame/callsame, the proto's own
# candidate set and the no-match error all stay as they are, call after call.

plan 11;

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
is $var, 42, 'a sigilless parameter aliases the caller variable';
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

{
    proto sub s4($) {*}
    multi sub s4(Int $x) { "outer" }
    my $inner;
    {
        proto sub s4(|) {*}
        multi sub s4(Int $x) { "inner" }
        $inner = (^120).map({ s4($_) }).unique.join;
    }
    is $inner, "inner", "an inner proto's candidates answer inside its block";
    is s4(5), "outer", "the outer proto comes back after that block";
}

my proto sub wrapped(|) {*}
my multi sub wrapped(Int $x) { $x * 2 }
&wrapped.wrap(-> |c { callsame() + 1 });
is (^120).map({ wrapped($_) }).sum, (^120).map({ $_ * 2 + 1 }).sum, 'a wrapped proto runs its wrapper';
