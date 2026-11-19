use Test;

# A closure that reads an enclosing sigilless binding (`\inner`) keeps reading
# that binding when a nested block of the same routine, later in the source,
# declares its own `my \inner` (#18176). It used to capture the routine's
# still-unset slot for the later declaration and read Nil.

plan 5;

my \inner = 'outer';
sub c() {
    my $o = { inner };
    my @r = $o();
    {
        my \inner = 'block';
        @r.push: $o();
        my $i = { inner };
        @r.push: $i();
    }
    @r.join(',')
}
is c(), 'outer,outer,block', 'the closure keeps the outer binding; one in the block sees the block';

my \Inner = 'OUT';
sub b() { my $o = { Inner }; my @r = $o(); { my \Inner = 'IN'; @r.push: $o() }; @r.join(',') }
is b(), 'OUT,OUT', 'an uppercase sigilless binding';

sub e() { my \x = 5; my $c = { x * 2 }; $c() }
is e(), 10, 'a binding declared before the closure is captured';

my @cl;
for 1..3 -> $i { my \v = $i * 10; @cl.push: { v } }
is @cl>>.().join(','), '10,20,30', 'each loop iteration keeps its own binding';

sub f() { my $c; { my \y = 7; $c = { y } }; $c() }
is f(), 7, 'a block binding read after the block exits';
