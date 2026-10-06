use Test;

plan 6;

# Cool.int8 & co. are `self.Numeric.int8`: an aggregate numifies to its
# element count first.
is [1, 2].int8, 2, 'Array.int8 is the element count';
is [1, 2, 3].uint8, 3, 'Array.uint8 is the element count';
is %(a => 1).int64, 1, 'Hash.int64 is the element count';
is (1, 2).byte, 2, 'List.byte is the element count';
is [1, 2, 3].uint16, 3, 'Array.uint16 is the element count';
is [].int32, 0, 'empty Array.int32 is 0';
