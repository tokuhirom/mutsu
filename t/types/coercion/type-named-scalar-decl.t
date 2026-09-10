use v6;
use Test;

plan 5;

# `my $Buf = Buf.new` — the declaration pre-seeds env with a placeholder
# before the RHS runs; the RHS bareword `Buf` must still resolve to the
# TYPE, not the placeholder (URI::Escape's `my $Buf = Buf.new` loop).
my $Buf = Buf.new;
isa-ok $Buf, Buf, 'RHS bareword resolves to the type in its own declaration';
$Buf.push('ab'.encode('utf8'));
is $Buf.elems, 2, 'the constructed Buf works';

my $Str = Str;
ok $Str === Str, 'a type-named scalar can hold the type object';

my $Int = Int.new(42);
is $Int, 42, 'Int-named scalar constructs an Int';

# The distinct-symbol rule still holds after assignment.
my $Bag = 5;
is $Bag + 1, 6, 'a type-named scalar holds its assigned value';
