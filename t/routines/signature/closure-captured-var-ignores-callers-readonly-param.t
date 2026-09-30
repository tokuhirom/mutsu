use v6;
use Test;

plan 17;

# A variable a closure (or a nested `my sub`) captures belongs to the routine
# that CREATED it. A readonly (non-`is rw`) parameter of whoever happens to
# CALL the code object later must not make an assignment to that captured
# variable fail just because the two share a name (#10389; found via
# Terminal::ANSIParser, whose parser callbacks assign their own `my Buf
# $string` while the test helper that drives them has a `$string` parameter).

sub make-buf-pair() {
    my Buf $string;
    my sub start() { $string = buf8.new(1) }
    my sub get() { $string }
    &start, &get;
}
sub drive-buf($string) {
    my ($start, $get) = make-buf-pair();
    $start();
    $get().raku;
}
is drive-buf(5), 'Buf[uint8].new(1)', 'my sub assigns its own captured typed scalar';

sub make-anon-sub-pair() {
    my $string;
    my &start = sub { $string = 1 };
    my &get = sub { $string };
    &start, &get;
}
sub drive-anon-sub($string) {
    my ($start, $get) = make-anon-sub-pair();
    $start();
    $get();
}
is drive-anon-sub(5), 1, 'anonymous sub assigns its own captured scalar';

sub make-block-pair() {
    my $string;
    my $start = { $string = 2 };
    my $get = { $string };
    $start, $get;
}
sub drive-block($string) {
    my ($start, $get) = make-block-pair();
    $start();
    $get();
}
is drive-block(5), 2, 'bare block assigns its own captured scalar';

sub make-pointy-pair() {
    my $string;
    my $start = -> { $string = 3 };
    my $get = -> { $string };
    $start, $get;
}
sub drive-pointy($string) {
    my ($start, $get) = make-pointy-pair();
    $start();
    $get();
}
is drive-pointy(5), 3, 'pointy block assigns its own captured scalar';

sub make-nested-sub-pair() {
    my $string;
    sub start() { $string = 4 }
    sub get() { $string }
    &start, &get;
}
sub drive-nested-sub($string) {
    my ($start, $get) = make-nested-sub-pair();
    $start();
    $get();
}
is drive-nested-sub(5), 4, 'plain nested sub assigns its own captured scalar';

sub make-incrementer() {
    my $n = 0;
    my sub bump() { $n++ }
    my sub current() { $n }
    &bump, &current;
}
sub drive-incrementer($n) {
    my ($bump, $current) = make-incrementer();
    $bump();
    $bump();
    $current();
}
is drive-incrementer(5), 2, 'postfix ++ on a captured scalar is not refused';

sub make-counter() {
    my $count = 0;
    return { $count++ }, { $count };
}
sub count-a($count) {
    my ($inc, $get) = make-counter();
    $inc() for ^3;
    $get();
}
sub count-b($count is copy) {
    my ($inc, $get) = make-counter();
    $inc() for ^2;
    $get();
}
is count-a(42), 3, 'closure pair created elsewhere counts under a readonly same-named param';
is count-b(42), 2, 'closure pair created elsewhere counts under an is-copy same-named param';

# The closure's own view wins over a loop alias that shadows the name.
{
    my $n = 0;
    my $inc = { $n++ };
    for 1..3 -> $n { $inc() }
    sub shadowing-param($n) { $inc() }
    shadowing-param(10);
    is $n, 4, 'a same-named loop alias and parameter do not block a captured scalar';
}

# Two levels of nesting.
sub make-two-level() {
    my $v = 0;
    my $make = { -> { $v = 7 } };
    my $set = $make();
    return { $set(); $v };
}
sub drive-two-level($v) { make-two-level()() }
is drive-two-level(3), 7, 'a closure nested in a closure assigns the outer routine\'s scalar';

# What must keep failing: the captured variable IS the readonly parameter.
sub readonly-param-same-frame($x) {
    my $c = { $x = 9 };
    try { $c(); CATCH { default { return .message } } }
    'no error';
}
like readonly-param-same-frame(1), /'readonly'/,
    'closure assigning its own routine\'s readonly parameter still fails';

sub make-escaping-closure($x) { return { $x = 1 } }
sub call-escaped-closure() {
    my $escaped = make-escaping-closure(5);
    try { $escaped(); CATCH { default { return .message } } }
    'no error';
}
like call-escaped-closure(), /'readonly'/,
    'the readonly state travels with an escaped closure after its routine returned';

sub readonly-param-nested-sub($x) {
    my sub inner() { $x = 1 }
    my &value = &inner;
    try { value(); CATCH { default { return .message } } }
    'no error';
}
like readonly-param-nested-sub(1), /'readonly'/,
    'nested sub called as a value still refuses its routine\'s readonly parameter';

sub copy-param($x is copy) {
    my $c = { $x = 9 };
    $c();
    $x;
}
is copy-param(1), 9, 'is copy parameter stays assignable through a closure';

my $rw = 1;
sub rw-param($x is rw) {
    my $c = { $x = 9 };
    $c();
}
rw-param($rw);
is $rw, 9, 'is rw parameter stays assignable through a closure';

# The topic is managed separately and is not touched by the reconciliation.
{
    my @items = 1, 2;
    my $ok = True;
    for @items { my $c = { $_ = 3 }; $c(); CATCH { default { $ok = False } } }
    ok $ok, 'a block assigning its own topic is unaffected';
    is @items, [3, 3], 'the topic assignment reached the array elements';
}
