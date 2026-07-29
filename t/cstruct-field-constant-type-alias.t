use v6;
use Test;
use NativeCall;

# A C binding names its platform-dependent types once and reuses them, so a
# CStruct field's declared type is routinely a `constant` alias rather than a
# NativeCall type name. DBIish's `MYSQL_BIND` is the motivating case:
#
#   constant my_bool = int8;
#   constant intptr  = ptrsize == 8 ?? uint64 !! uint32;
#   class MYSQL_BIND is repr('CStruct') { has intptr $.length is rw; ... }
#
# An unresolved field type aborts the whole layout, so the struct had no layout
# at all and `nativesizeof` failed on it.

plan 6;

constant my_bool = int8;
constant word = uint64;

class Aliased is repr('CStruct') {
    has int32   $.a is rw;
    has my_bool $.b is rw;
}

class Wide is repr('CStruct') {
    has word  $.a is rw;
    has int32 $.b is rw;
}

class Mixed is repr('CStruct') {
    has Pointer[my_bool] $.p;
    has my_bool          $.flag is rw;
}

is Aliased.REPR, 'CStruct', 'a class with an aliased field type is still a CStruct';
is nativesizeof(Aliased), 8, 'int32 + int8 field, padded to the 4-byte alignment';
is nativesizeof(Wide), 16, 'uint64 + int32 field, padded to the 8-byte alignment';
is nativesizeof(Mixed), 16, 'a typed pointer next to an aliased scalar field';

# The alias resolves to the same C type the spelled-out name would, so the
# layout is identical.
class Spelled is repr('CStruct') {
    has int32 $.a is rw;
    has int8  $.b is rw;
}
is nativesizeof(Aliased), nativesizeof(Spelled), 'an alias lays out like its target type';

# A chain of aliases resolves too.
constant flag = my_bool;
class Chained is repr('CStruct') {
    has int32 $.a is rw;
    has flag  $.b is rw;
}
is nativesizeof(Chained), 8, 'a chained constant alias resolves';
