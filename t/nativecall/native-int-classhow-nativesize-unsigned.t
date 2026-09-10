use Test;

plan 12;

is int8.^nativesize, 8, 'int8.^nativesize';
is int16.^nativesize, 16, 'int16.^nativesize';
is int32.^nativesize, 32, 'int32.^nativesize';
is int64.^nativesize, 64, 'int64.^nativesize';
is uint8.^nativesize, 8, 'uint8.^nativesize';
is uint64.^nativesize, 64, 'uint64.^nativesize';

is int8.^unsigned, 0, 'int8.^unsigned is signed';
is int64.^unsigned, 0, 'int64.^unsigned is signed';
is uint8.^unsigned, 1, 'uint8.^unsigned is unsigned';
is uint64.^unsigned, 1, 'uint64.^unsigned is unsigned';

dies-ok { Int.^nativesize }, 'Int.^nativesize dies (not a native type)';
dies-ok { Str.^unsigned }, 'Str.^unsigned dies (not a native type)';
