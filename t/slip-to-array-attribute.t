use Test;

# A `|@x` slip (or a Seq) passed to an `@`-sigiled attribute is flattened into
# the array container, exactly like `my @a = |@x` yields an Array (not a Slip).
# Surfaced by zef: `Zef::Fetch.new(:backends(|%config<Fetch>))` left `@.backends`
# as a one-shot Slip whose `.^name` was `Slip` instead of `Array`.

plan 8;

class Simple { has @.b }          # default constructor path
class Complex { has @.b; submethod TWEAK {} }   # full dispatch_new path

my @x = (1, 2, 3);

is Simple.new(:b(|@x)).b.^name, 'Array', 'slip into @-attr is Array (default ctor)';
is Simple.new(:b(|@x)).b.elems, 3, 'slip into @-attr keeps all elements';
is-deeply Simple.new(:b(|@x)).b.List, (1, 2, 3), 'slip into @-attr has correct elements';

is Complex.new(:b(|@x)).b.^name, 'Array', 'slip into @-attr is Array (dispatch_new)';

# A Seq is likewise materialized into the array.
is Simple.new(:b((1, 2).Seq)).b.^name, 'Array', 'Seq into @-attr is Array';

# Re-iteration works (an Array, unlike a one-shot Slip, can be walked twice).
my $c = Complex.new(:b(|@x));
is $c.b.elems, 3, 'first read of @-attr';
is $c.b.elems, 3, 'second read of @-attr (re-iterable)';

# A plain array argument is unchanged.
is Simple.new(:b([4, 5])).b.^name, 'Array', 'plain array @-attr stays Array';
