use Test;

# A closure's store is checked against the type of the variable it names, not
# against a same-named variable of a frame that happens to sit between the
# closure and its definition on the call chain (ADR-13757 step 4).

plan 4;

# The caller binds an untyped `$v` over a typed one further down; the closure
# stores into its own (untyped) captured `$v`.
my $v = 0;
my &store = -> { $v = 's' };
sub untyped-middle() { my $v = 1; store(); $v }
sub typed-bottom() { my Int $v = 2; untyped-middle() }
is typed-bottom(), 1, 'the middle frame keeps its own $v';
is $v, 's', 'the closure stored into its captured untyped $v';

# The same with an array.
my @a;
my &push-it = -> { @a.push('s') };
sub untyped-mid-array() { my @a = 1, 2; push-it(); @a.elems }
sub typed-bottom-array() { my Int @a = 3; untyped-mid-array() }
is typed-bottom-array(), 2, 'the middle array is untouched';
is @a.elems, 1, 'the closure pushed onto its captured untyped @a';
