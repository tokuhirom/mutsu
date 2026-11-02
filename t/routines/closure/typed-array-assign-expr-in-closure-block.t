use Test;

# Found via Fasta: `@counts = ()` as the tail of an `if` block inside a
# closure is an assignment used as an expression; it reaches the captured
# array by name and used to drop the declared `array[int]` / `Array[Int]` type.
plan 5;

my int @n = 0, 5;
sub reset-n() { if @n { @n = () } }
reset-n;
is @n.WHAT.raku, 'array[int]', 'native array keeps its type after an assignment in an if block';
@n[2] = 7;
is @n.raku, 'array[int].new(0, 0, 7)', 'and it still stores native ints';

my Int @t = 1, 2;
sub reset-t() { if True { @t = () } }
reset-t;
is @t.WHAT.raku, 'Array[Int]', 'Array[Int] keeps its type';

my @plain = 1, 2;
sub reset-p() { if True { @plain = () } }
reset-p;
is @plain.WHAT.raku, 'Array', 'an untyped array stays plain';

my Int @u = 1;
sub bad() { if True { @u = "x", "y" } }
dies-ok { bad() }, 'the element type is still enforced';
