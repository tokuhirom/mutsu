use Test;

# `@a.push` compiles to the `ArrayPush` opcode only for a single-arg push on a
# *local* array. The captured-closure form and the multi-arg form reach the VM
# as `CallMethodMut`; this pins the VM-native fast path for those (push added to
# `try_native_array_mut`).

plan 18;

# multi-arg push on a local array (CallMethodMut, not ArrayPush)
my @a;
@a.push(1, 2, 3);
is @a.join(','), '1,2,3', 'multi-arg push on local array';
@a.push(4);
is @a.join(','), '1,2,3,4', 'subsequent push grows';

# captured-closure single-arg push
my @b;
my $c = { @b.push($_) };
$c(10);
$c(20);
$c(30);
is @b.join(','), '10,20,30', 'captured single-arg push accumulates';

# captured-closure multi-arg push
my @d;
my $e = { @d.push($_, $_ * 2) };
$e(5);
$e(6);
is @d.join(','), '5,10,6,12', 'captured multi-arg push';

# push via map closure
my @f;
(1..3).map({ @f.push($_ * 100) });
is @f.join(','), '100,200,300', 'push inside map closure';

# Slip flattening
my @g;
@g.push(|(7, 8, 9));
is @g.join(','), '7,8,9', 'push flattens a Slip argument';

# Empty pushes nothing
my @h;
@h.push(Empty);
is @h.elems, 0, 'push Empty adds nothing';
@h.push(1);
@h.push(Empty);
is @h.elems, 1, 'push Empty after a real element is a no-op';

# push returns the array
my @z;
my $ret = @z.push(99);
is $ret.^name, 'Array', 'push returns the Array';
is $ret.elems, 1, 'returned array has the pushed element';

# Typed array push still type-checks (falls through to the interpreter)
my Int @t;
@t.push(1, 2);
is @t.join(','), '1,2', 'typed array multi-arg push works';
my $threw = False;
{
    @t.push("not an int");
    CATCH { default { $threw = True } }
}
ok $threw, 'typed array push rejects a wrong-typed value';

# Captured typed array push also type-checks
my Int @u;
my $up = { @u.push($_) };
$up(1);
$up(2);
is @u.join(','), '1,2', 'captured typed array push works';

# Mix of itemized and plain arguments
my @v;
@v.push(1, $(2, 3), 4);
is @v.elems, 3, 'itemized argument pushed as a single element';
is @v[1].elems, 2, 'itemized element preserved';

# Nested array values are pushed as-is (no flattening of a non-Slip Array arg)
my @w;
my @inner = 1, 2, 3;
@w.push(@inner);
is @w.elems, 1, 'pushing an array adds one element';
is @w[0].elems, 3, 'pushed array element keeps its contents';

# Loop pushes
my @s;
@s.push($_) for 1..5;
is @s.join(','), '1,2,3,4,5', 'for-loop push accumulates';

done-testing;
