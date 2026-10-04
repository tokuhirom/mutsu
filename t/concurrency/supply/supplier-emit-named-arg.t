use Test;

plan 3;

# A bareword `a => 1` is a named argument, so `emit` has no positional value.
my $s = Supplier.new;
my @got;
$s.Supply.tap({ @got.push($_) });

dies-ok { $s.emit(a => 1) }, 'Supplier.emit(a => 1) fails: named arg is not a value';
$s.emit((a => 1));
is @got.elems, 1, 'a parenthesised pair is emitted as a value';
is @got[0].key, 'a', 'the emitted value is the Pair';
