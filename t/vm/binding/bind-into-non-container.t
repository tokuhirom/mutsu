use Test;

# Binding a positional/associative element into a target that is not a bindable
# container must throw X::Bind. A type object (`(List)`, `(Int)`) has no element
# slot to bind, so `(List)[0] := 1` used to silently succeed (the generic
# index-assign fell through to a no-op arm). It now throws X::Bind, matching
# roast/S32-exceptions/misc.t (which asserts X::Bind for these). A *defined* Int
# / Str LHS (`10[0] := 1`, `"Hi"[0] := 1`) is already rejected at parse time as
# an invalid bind left-hand side, also surfacing as X::Bind.

plan 7;

throws-like '(List)[0] := 1', X::Bind, 'bind into an undefined List type object';
throws-like '(Int)[0]  := 1', X::Bind, 'bind into an undefined Int type object';
throws-like '(Any)[0]  := 1', X::Bind, 'bind into an undefined Any type object';
throws-like '10[0]     := 1', X::Bind, 'bind into a defined Int';
throws-like '"Hi"[0]   := 1', X::Bind, 'bind into a defined Str';

# A genuine bindable container still binds (no false X::Bind).
{
    my @a;
    @a[0] := my $x;
    $x = 42;
    is @a[0], 42, 'binding into an Array element still works (aliases the source)';
}
{
    my %h;
    %h<k> := my $y;
    $y = 7;
    is %h<k>, 7, 'binding into a Hash element still works (aliases the source)';
}
