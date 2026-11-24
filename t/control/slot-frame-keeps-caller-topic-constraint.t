use Test;

# #19146: a leaf method that runs in the slot method frame (ADR-18393) runs on
# its caller's env handle. Entering it must not strip the caller's typed-`$_`
# constraint: the caller's `$_` keeps its type after the call. Debug builds
# also assert that the env handle is unchanged at return.

plan 4;

class W {
    method n($) { 'null' }
    method sum($a, $b) { $a + $b }
}
my $w = W.new;

sub typed-topic(Int $_ is copy) {
    $w.n(1) for ^3;
    $_ = 'str';
    'assigned'
}
throws-like { typed-topic(1) }, X::TypeCheck::Assignment,
    'the caller keeps its Int $_ constraint after a slot-frame call';

sub typed-topic-sum(Int $_ is copy) {
    my $total = 0;
    $total = $w.sum($total, $_) for ^3;
    $_ = 'str';
    $total
}
throws-like { typed-topic-sum(2) }, X::TypeCheck::Assignment,
    '... also after a slot-frame call that adds its arguments';

sub still-assignable(Int $_ is copy) {
    $w.n(1) for ^3;
    $_ = 42;
    $_
}
is still-assignable(1), 42, 'an assignment the constraint accepts still works';

is (^3).map({ $w.n($_) }).join(','), 'null,null,null',
    'a slot-frame call inside a block with an untyped $_';
