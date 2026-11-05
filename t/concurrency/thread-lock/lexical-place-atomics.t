use v6;
use Test;

# ADR-16595 §2 rule 3: an atomic or structural operation on a plain user
# lexical addresses the container's place (its slot or the cell every capture
# shares), never its bare name. Two live bindings of one name -- an inner
# block's `my @d` over an outer `@d`, a worker's own `my @x` next to the
# parent's, sibling workers' `my @w` -- are two containers and must stay so
# once a thread has run.

plan 14;

await start { 1 };   # arm the cross-thread machinery for the whole file

# --- an inner `my @d` shadowing an outer `@d` ---------------------------------

my @d = 1, 1;
{
    my atomicint @d = 9, 9;
    cas(@d[0], 9, 3);
    is @d.join(","), "3,9", 'cas on the inner shadowing array lands in the inner array';
    @d[1]⚛++;
    is @d.join(","), "3,10", 'an element increment lands in the inner array';
}
is-deeply @d, [1, 1], 'the outer array is untouched by the inner one';

my %h = a => 1;
{
    my %h = a => 5;
    cas(%h<a>, 5, 6);
    is %h<a>, 6, 'cas on the inner shadowing hash lands in the inner hash';
}
is %h<a>, 1, 'the outer hash is untouched by the inner one';

# --- a worker's own `my @shadowed` next to the parent's -----------------------

my @shadowed = <PARENT>;
my %shadowed-h = p => 1;
is (await start { my @shadowed; @shadowed.push('w'); @shadowed.join(',') }), 'w',
    "a worker's own array starts empty and takes its own push";
is @shadowed.join(','), 'PARENT', "the parent's array is untouched";
is (await start { my %shadowed-h; %shadowed-h<w> = 1; %shadowed-h.keys.join(',') }), 'w',
    "a worker's own hash takes its own element store";
is %shadowed-h.keys.join(','), 'p', "the parent's hash is untouched";

# --- sibling workers' own `my @w` ---------------------------------------------

my @res = await (^3).map: -> $n { start { my @w = ($n,); @w.push($n * 10); @w.join(',') } };
is @res.join('|'), '0,0|1,10|2,20', "sibling workers' same-named arrays stay isolated";

my @cas = await (^3).map: -> $n { start { my @w = 0, 0; cas(@w[1], 0, $n + 1); @w.join(',') } };
is @cas.join('|'), '0,1|0,2|0,3', "sibling workers' element cas stays in each worker's array";

# --- an atomic element increment on a shared captured array -------------------

my atomicint @a = 0, 0;
await (^8).map: { start { @a[1]⚛++ for ^200 } };
is @a[1], 1600, 'concurrent element increments on a captured array lose no update';
is @a[0], 0, '... and leave the other element alone';

# --- a capture held by a thunk that holds the spawning closure ----------------
# `start { ... } xx $n` runs the `start` from a thunk; the array is captured by
# a closure two levels down and must still be one shared container.

my $copies = 3;
my $lock = Lock::Async.new;
my @pushed;
await start { $lock.protect: { push @pushed, 1 } } xx $copies;
is @pushed.elems, 3, 'pushes from workers started through an xx thunk all land';

done-testing;
