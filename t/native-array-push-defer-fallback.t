use Test;
plan 16;

# ADR-0019 E9-pre ticket: nextsame/callsame from a user-overridden array
# mutator method on a class that `is Array` must reach the real native
# array mutation (push/append/prepend/unshift/pop/shift), not silently do
# nothing. Raku-verified expected output (Rakudo v2026.06):
#
#   class MyArr is Array {
#       method push(|c) { nextsame }
#   }
#   my $a = MyArr.new;
#   $a.push(1);
#   $a.push(2, 3);
#   say $a.elems;   # 3

class MyArrPush is Array {
    method push(|c) { nextsame }
}

{
    my $a = MyArrPush.new;
    $a.push(1);
    is $a.elems, 1, 'nextsame push: single element mutates the backing array';
    is-deeply $a.List, (1,), 'nextsame push: single element value';
}

{
    my $a = MyArrPush.new;
    $a.push(1);
    $a.push(2, 3);
    is $a.elems, 3, 'nextsame push: multiple elements across two calls';
    is-deeply $a.List, (1, 2, 3), 'nextsame push: values in order';
}

# callsame additionally must return the invocant (Raku's base Array.push
# returns self), preserving identity and the subclass type.
class MyArrCallsame is Array {
    method push(|c) { my $r = callsame; $r }
}

{
    my $b = MyArrCallsame.new;
    my $ret = $b.push(10);
    is $b.elems, 1, 'callsame push: mutates the backing array';
    is-deeply $b.List, (10,), 'callsame push: value';
    ok $ret === $b, 'callsame push: return value is the SAME invocant (identity)';
    is $ret.^name, 'MyArrCallsame', 'callsame push: return value keeps the subclass type';
}

# pop/shift/unshift/append go through the same synthesized fallback and
# share the same root cause — cover them together.
class MyArrOthers is Array {
    method pop(|c) { nextsame }
    method shift(|c) { nextsame }
    method unshift(|c) { nextsame }
    method append(|c) { nextsame }
}

{
    my $c = MyArrOthers.new;
    $c.append(1, 2, 3);
    is-deeply $c.List, (1, 2, 3), 'nextsame append: values';

    my $popped = $c.pop;
    is $popped, 3, 'nextsame pop: removed element value';
    is-deeply $c.List, (1, 2), 'nextsame pop: remaining elements';

    $c.unshift(0);
    is-deeply $c.List, (0, 1, 2), 'nextsame unshift: prepended element';

    my $shifted = $c.shift;
    is $shifted, 0, 'nextsame shift: removed element value';
    is-deeply $c.List, (1, 2), 'nextsame shift: remaining elements';
}

# nextwith with explicit (overridden) args must still work (pre-existing
# coverage via t/array-subclass-vector.t; pinned here too as a boundary
# check against the args-recovery fix above).
class MyArrNextwith is Array {
    method push(|c) { nextwith(99) }
}

{
    my $d = MyArrNextwith.new;
    $d.push(1);
    is-deeply $d.List, (99,), 'nextwith push: overridden args win over original call args';
}

# Sanity: a plain (non-overridden) Array still behaves normally.
{
    my @plain = 1, 2;
    @plain.push(3);
    is-deeply @plain, [1, 2, 3], 'plain Array push is unaffected';
}
