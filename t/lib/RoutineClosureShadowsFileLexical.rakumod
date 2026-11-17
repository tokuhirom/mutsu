unit module RoutineClosureShadowsFileLexical;

# File-scope variables that the routines below shadow with their own `my`,
# loop parameter or `our`-named lexical (#17466).
my $x = 'file';
our $o = 'our-file';

# A pointy loop parameter read by a closure that runs after the loop ended.
our sub loop-closures is export { my @c; for 1, 2 -> $x { @c.push: { $x } }; @c.map({ .() }).join(',') }
# A loop parameter read by an anonymous sub called inside the loop.
our sub loop-sub is export { my @r; for 1 -> $x { @r.push: (sub { $x })() }; @r.join(',') }
# A `<->` loop parameter written through.
our sub loop-rw is export { my @a = 1, 2; for @a <-> $x { $x *= 10 }; @a.join(',') }
# A routine's own `my` read by a block, a pointy block and an anonymous sub.
our sub block-read is export { my $x = 'inner'; my $f = { $x }; $f() }
our sub pointy-read is export { my $x = 'inner'; my $f = -> { $x }; $f() }
our sub sub-read is export { my $x = 'inner'; my $f = sub { $x }; $f() }
our sub escaped-read is export { my $x = 'inner'; my $f = { $x }; $f }
our sub nested-read is export { my $x = 'inner'; my $c = -> { -> { $x } }; $c()() }
our sub block-local-read is export { my @c; for 1, 2 { my $x = $_; @c.push: { $x } }; @c.map({ .() }).join(',') }
# Writes through a closure reach the routine's variable.
our sub block-write is export { my $x = 1; my $c = { $x = 5 }; $c(); $x }
our sub block-increment is export { my $x = 1; my $c = { $x++ }; $c(); $c(); $x }
our sub hash-write is export { my %x; my $c = { %x<a> = 1 }; $c(); %x.keys.join(',') }
our sub array-write is export { my @x; my $c = { @x.push: 3 }; $c(); @x.join(',') }
# A routine's `my $o` shadows the package's `our $o`, for reads and writes.
our sub our-shadow-read is export { my $o = 'mine'; my $c = { $o }; $c() }
our sub our-shadow-write is export { my $o = 1; my $c = { $o = 9 }; $c(); $o }
# A closure in a routine that does not declare the name still reaches the
# file scope.
our sub file-read is export { my $c = { $x }; $c() }
our sub our-read is export { my $c = { $o }; $c() }
# A sibling block's `my $x` has ended: the closure means the file's `$x`.
our sub sibling-block-read is export { { my $x = 'block' }; my $c = { $x }; $c() }
