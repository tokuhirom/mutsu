use Test;

# A bare block's exit re-syncs a `:=` alias's target with the local the block
# modified. The exit used to visit every local of the frame once any alias
# existed in the process; it now visits only the slots whose alias key has ever
# been bound (#9170). The aliases below must still see every write.

plan 7;

my @filler = (^50).map({ "f$_" });
my ($f0, $f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8, $f9) = @filler;

my $target = 1;
my $alias := $target;
{ $alias = 2 }
is $target, 2, 'a write through the alias inside a block reaches the target';

{ $target = 3 }
is $alias, 3, 'a write to the target inside a block reaches the alias';

my $other = 'a';
my $other-alias := $other;
for ^3 -> $i { { $other-alias ~= $i } }
is $other, 'a012', 'a loop of blocks keeps the second alias in sync';
is $alias, 3, 'the first alias is untouched by the second';

sub in-routine {
    my $x = 10;
    my $y := $x;
    { $y += 5 }
    { $x *= 2 }
    "$x/$y"
}
is in-routine(), '30/30', 'aliases in a routine frame stay in sync across blocks';

my @list = 1, 2;
my @list-alias := @list;
{ @list-alias.push: 3 }
is @list.join(','), '1,2,3', 'an array alias';

for ^2 {
    my $loop-x = 'p';
    my $loop-y := $loop-x;
    { $loop-y ~= 'q' }
    is $loop-x, 'pq', 'an alias declared in a loop body' if $_ == 1;
}
