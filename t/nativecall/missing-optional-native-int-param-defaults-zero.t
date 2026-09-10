use Test;

plan 4;

# A native int type has no undefined state (`my int8 $x;` is 0, not an
# uninitialized Int), so an unpassed optional native-int parameter must bind
# the native zero, not a generic type-object placeholder.

sub named-int8(int8 :$id) { $id }
is named-int8(), 0, 'unpassed named int8 param defaults to 0';

sub positional-optional-int32(int32 $y?) { $y }
is positional-optional-int32(), 0, 'unpassed optional positional int32 param defaults to 0';

my $blk = -> int8 :$id { $id };
is $blk(), 0, 'unpassed named int8 block param defaults to 0';

sub named-int8-passed(int8 :$id) { $id }
is named-int8-passed(id => 5), 5, 'passed named int8 param still works';
