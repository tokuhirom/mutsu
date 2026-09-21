# `try_fast_hash_element_assign` is consulted BEFORE the element store's shared
# preamble now (#8069), the same reordering the positional lane got in #8151.
# The preamble used to establish several facts on the lane's behalf, so each
# block below is one of those facts, now guarded by the lane's own wrapper
# (`try_fast_hash_element_assign_early`).
#
# All expectations were taken from real rakudo.
use Test;
plan 14;

# --- the shape the lane serves --------------------------------------------

{
    my %h;
    %h<a> = 1;
    %h{'b'} = 2;
    %h{3} = 4;
    is %h<a> ~ %h<b> ~ %h{3}, '124', 'plain hash element stores';
    is (%h<c> = 9), 9, 'a hash element store evaluates to the assigned value';
}

# --- facts the preamble used to establish ---------------------------------

{
    # ADR-0040 §9 from the destination side: a Proxy element mediates its own
    # store. The preamble's `existing_element_container` probe caught this
    # before the lane was ever reached; the lane asks for itself now.
    my %h;
    my $backing = 0;
    %h<p> := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v * 10 });
    %h<p> = 7;
    is $backing, 70, 'a Proxy hash element still mediates its own store';
    is %h<p>, 70, 'and reading it goes through FETCH';
}

{
    # `%h{*} = ...` has no order to assign across; rakudo refuses it. The
    # refusal lives in the preamble, so the lane must not serve a Whatever
    # subscript.
    my %w;
    my $msg = '';
    { %w{*} = 5; CATCH { default { $msg = .message } } }
    is $msg, 'Cannot assign to *, as the order of keys is non-deterministic',
        'a Whatever subscript is still refused';
    is %w.elems, 0, 'and nothing was written under a stringified "*" key';
}

{
    # A `Pair` reached by an associative subscript takes a whole-container
    # store into its value, not an element store. Handled above the lane.
    my $p = (c => [1, 2]);
    $p<c> = [3, 4];
    is $p.raku, ':c([3, 4])', 'a Pair subscript is still a whole-container store';
}

{
    # A `:=`-bound element is a shared cell: the store writes through it.
    my $s = 5;
    my %b;
    %b<x> := $s;
    %b<x> = 9;
    is $s, 9, 'a := bound hash element is written through';
    is %b<x>, 9, 'and reads back through the same cell';
}

{
    # `is default` decides what a Nil store writes (ADR-0049).
    my %d is default(42);
    %d<k> = 1;
    %d<k> = Nil;
    is %d<k>, 42, 'Nil decays to the container default';
    is %d<absent>, 42, 'and an absent key still reads the default';
}

{
    # A typed hash needs the full path's constraint check.
    my Int %t;
    %t<a> = 3;
    is %t<a>, 3, 'a typed hash accepts a matching value';
    my $died = False;
    { %t<b> = 'str'; CATCH { default { $died = True } } }
    ok $died, 'and still refuses a mismatching one';
}

{
    # A hash held by a `$` is reached through the scalar slot, which the
    # preamble seeds env from; the lane reads env, so it must decline. A Map is
    # immutable, so the store has to throw rather than silently replace it.
    my $m = {a => 1}.Map;
    my $died = False;
    { $m<a> = 2; CATCH { default { $died = True } } }
    ok $died, 'a store into a scalar-held Map still throws';
}
