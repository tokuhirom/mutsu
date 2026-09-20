use Test;

# Protocol::Postgres uses Hash[K, V].keyof while constructing a packet schema.
plan 2;

is Hash[Str, Str].keyof.^name, 'Str', 'Hash[K, V].keyof returns K';
is Hash[Int, Str].keyof.^name, 'Str', 'Hash[K, Str].keyof returns Str';
