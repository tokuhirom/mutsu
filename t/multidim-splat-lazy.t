use v6;
use Test;

plan 14;

# Pin for `%h{|| @list}` (dimension splat over a hash) and unbounded-end range
# dimensions (`@a[1^..*;1]`) — the remaining plan-mismatch blocks of
# S32-hash/multislice-6e.t.

# --- hash dimension splat: %h{|| @indices} ---
{
    my %hash;
    my @indices := <a b>, <c>;   # need binding, no containers!

    is-deeply (%hash{|| @indices} = 42,666), (42,666),
      '|| assignment to non-existing hashes returns the assigned values';
    is-deeply %hash, { a => { c => 42 }, b => { c => 666 } },
      '|| initialization works';
    is-deeply (%hash{|| @indices}:delete), (42,666),
      '|| deletion returns the expected values';
    is-deeply %hash, { a => { }, b => { } },
      '|| hash changed correctly after delete';
    is-deeply (%hash{|| <a b>, "d"} = 333, 444), (333,444),
      '|| assignment with an inline dimension list';
    is-deeply %hash, { a => { d => 333 }, b => { d => 444 } },
      '|| hash changed correctly';
    is-deeply %hash{|| "b"}, { d => 444 },
      '|| single key handled correctly';
}

# --- unbounded-end range dimensions ---
{
    my @a = [1,2,3],[4,5,6],[7,8,9],[10,11,12];
    is-deeply @a[1^..*;1], (8,11),
      'excl-start Whatever-ended range dimension';
    is-deeply @a[1..*;1], (5,8,11),
      'Whatever-ended range dimension';
    is-deeply @a[1^..*;1]:exists, (True,True),
      'unbounded range dimension with :exists';
    is-deeply @a[1^..*;1]:delete, (8,11),
      'unbounded range dimension with :delete';
    is-deeply @a, [[1,2,3],[4,5,6],[7,Any,9],[10,Any,12]],
      'proper elements were deleted';
    is-deeply @a[1^..*;1]:exists, (False,False),
      ':exists reports False for deleted slots';
    is-deeply @a[1^..*;1], (Any,Any),
      'deleted slots read back as holes';
}

# vim: expandtab shiftwidth=4
