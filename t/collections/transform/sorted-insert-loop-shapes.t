use v6;
use Test;
use nqp;

# The shapes of Array::Sorted::Util's binary-search / sorted-insert loop
# (Concurrent::PriorityQueue's `push`), which mutsu answers through a set of
# fast paths: a `:&cmp = &[cmp]` code parameter called per step, an
# `Order::Same` bareword compared with `nqp::eqaddr`, `($i = ...)` inside the
# loop condition, sigilless parameters bound to an array, and the
# arg-source names a call hands its binder (#15786).

plan 16;

# cmp answers the three Order values, each identical to its bareword.
{
    my @o = (1 cmp 2), (2 cmp 2), (3 cmp 2);
    is @o.map(*.Str).join(','), 'Less,Same,More', 'cmp answers Order values';
    ok nqp::eqaddr((1 cmp 2), Order::Less), 'Less is Order::Less (eqaddr)';
    ok (2 cmp 2) === Order::Same, 'Same === Order::Same';
    is (5 <=> 4), More, '<=> answers More';
    is ('a' leg 'b').^name, 'Order', 'leg answers an Order';
    is (1 cmp 2).value, -1, 'Less has value -1';
}

sub scan(\haystack, $needle, :&cmp = &[cmp]) {
    my int $i = haystack.elems;
    nqp::while(
      nqp::isge_i(($i = nqp::sub_i($i,1)),0)
        && nqp::eqaddr(cmp($needle,haystack.AT-POS($i)),Order::Same),
      nqp::null
    );
    $i
}

{
    my @a = 1, 7, 7, 7;
    is scan(@a, 7), 0, 'scan back over duplicates through &[cmp]';
    is scan(@a, 1), 3, 'scan stops at the first mismatch';
    is scan(@a, 7, :cmp(-> $a, $b { $b cmp $a })), 0, 'a user cmp block is called';
}

# A user `infix:<cmp>` candidate does not change what `&[cmp]` taken as a
# value answers for core types.
{
    my @a = 3, 3;
    my multi infix:<cmp>(Str $a, Str $b) { Order::Same }
    is scan(@a, 3), -1, '&[cmp] default with a user cmp candidate in scope';
}

# `($x = ...)` in an expression still tags its container for a `for`.
{
    my @seen;
    my @x;
    for (@x = 1, 2, 3) -> $v { @seen.push($v) }
    is @seen.join(','), '1,2,3', 'for over an assignment expression';
    my $y = 0;
    ($y = 5) += 1;
    is $y, 6, 'an assignment expression is still an lvalue';
}

# Sigilless parameters bound to an array see and change the caller's array.
sub ins(\haystack, $pos, \value) { haystack.splice($pos, 0, value); $pos }
sub outer(\h, $v) { ins(h, 0, $v) }
{
    my @a = 2, 3;
    outer(@a, 1);
    is @a.join(','), '1,2,3', 'a nested sigilless alias writes the caller array';
    my \b = @a;
    outer(b, 0);
    is @a.join(','), '0,1,2,3', 'an alias of an alias reaches the same array';
}

# The caller's variable names reach an `is rw` binder through a nested call.
sub bump($x is rw) { $x++ }
sub twice($y is rw) { bump($y); bump($y) }
{
    my $n = 1;
    twice($n);
    is $n, 3, 'is rw through two calls writes the caller variable';
}

# A module-style `my class` that is an Int, tested with nqp::istype.
{
    my class NotFound is Int { method defined(--> False) { } }
    ok !nqp::istype(5, NotFound) && nqp::istype(nqp::box_i(2, NotFound), NotFound),
      'nqp::istype against a lexical Int subclass';
}
