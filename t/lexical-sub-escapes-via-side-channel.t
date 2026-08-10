use Test;
plan 3;

# A routine that declares a lexical `sub` and hands a closure referencing it
# to a side channel (`.tap`, storing it in a variable) rather than returning
# it must keep the lexical sub callable when that closure runs later, after
# the declaring routine has already returned. The registry-restore gate that
# unregisters a call's lexical subs on return only recognized the *return
# value* as an escape route (`return_value_escapes_routine`); a closure
# literal created during the call and handed elsewhere was invisible to it,
# so the inner sub became `Unknown function` once the declaring routine's
# frame was gone. See
# todo/tickets/lexical-sub-lost-after-routine-return.md.

# Variant A: lexical sub in a plain sub; the returned closure invokes it
# after the declaring sub has returned.
sub make-closure() {
    sub helperA($x) { "A:$x" }
    return { helperA($_) };
}
{
    my $cb = make-closure();
    is $cb(42), 'A:42', 'closure returned from a sub still resolves its lexical sub';
}

# Variant B: TWEAK declares a lexical sub and taps a Supplier with a closure
# that calls it; the tap fires after TWEAK has returned.
class CB {
    has Supplier $.s = Supplier.new;
    has $.result;
    submethod TWEAK() {
        sub helperB($x) { "B:$x" }
        $!s.Supply.tap: { $!result = helperB($_) };
    }
}
{
    my $c = CB.new;
    $c.s.emit(42);
    is $c.result, 'B:42', 'tap callback registered in TWEAK resolves a lexical sub after TWEAK returns';
}

# Variant C: an ordinary method (not TWEAK) declares a lexical sub and taps a
# Supplier with a closure that calls it; the tap fires after the method
# returns.
class CC {
    has Supplier $.s = Supplier.new;
    has $.result;
    method setup() {
        sub helperC($x) { "C:$x" }
        $!s.Supply.tap: { $!result = helperC($_) };
    }
}
{
    my $c = CC.new;
    $c.setup;
    $c.s.emit(42);
    is $c.result, 'C:42', 'tap callback registered in a method resolves a lexical sub after the method returns';
}
