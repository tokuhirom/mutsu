use Test;

# #13748: Rakudo's `for` is `.map`, which reads `self.iterator`, and
# `Any.iterator` is `self.list.iterator`: an object whose class declares only
# `list` iterates its list, without composing `Iterable`. List assignment does
# not go through `.map`, so `@a = $obj` stays one element.

plan 5;

class WithList { method list { (3, 1, 2) } }

my @seen;
for WithList.new { @seen.push($_) }
is-deeply @seen, [3, 1, 2], 'for iterates the class\'s own list';

my $held = WithList.new;
my $n = 0;
$n++ for $held;
is $n, 1, 'a Scalar-held object is one item';

my @a = WithList.new;
is @a.elems, 1, 'list assignment keeps the object as one element';

class WithBoth { method list { (1, 2) }; method iterator { (7, 8, 9).iterator } }
my @both;
for WithBoth.new { @both.push($_) }
is-deeply @both, [7, 8, 9], 'its own iterator wins over its own list';

is WithList.new.are.^name, 'Int', '.are reads the list too';
