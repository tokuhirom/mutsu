use v6;
use Test;

# #8614: `dispatch_func_call_inner`'s interpreter-carrier fallback installs
# `pending_call_arg_sources` (the call site's own positional arg source
# names, used by a handful of natives like `tail` for their OWN args) before
# invoking a native builtin such as `reduce`, and only clears it once that
# builtin call has fully returned. `reduce` invokes its callback block once
# per item -- a NESTED call with its own, unrelated argument list -- and that
# nested call's parameter binder used to consume the still-installed, stale
# source-name list left over from `reduce`'s OWN call, binding a same-named
# variable's type constraint onto an unrelated untyped parameter of the
# block. The leaked constraint then stuck around in the caller's frame after
# the block returned, breaking an unrelated same-named variable there.

plan 2;

sub uses-typed-lambda(blob32 $M) {
    my $r = reduce -> blob32 $b, $i {
        blob32.new($b[0] + $i);
    }, $M, |^3;
    $r
}

is uses-typed-lambda(blob32.new(10, 20, 30, 40, 50))[0], 13,
    'the reduce block itself still computes the right accumulator';

sub outer() {
    my Int $i = 0;
    while $i < 3 {
        uses-typed-lambda(blob32.new(10, 20, 30, 40, 50));
        $i++;
    }
    $i
}

is outer(), 3,
    'a typed reduce-block parameter does not poison an unrelated same-named outer variable';
