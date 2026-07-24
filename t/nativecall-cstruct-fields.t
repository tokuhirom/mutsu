use Test;
use NativeCall;

# A `is repr('CStruct')` handle carries a C pointer; reading one of its declared
# attributes must read the field out of that struct, at the offset the C ABI
# gives it. Before this, every such accessor answered Nil.

plan 12;

# POSIX `struct tm` — its first nine members are ints in this order on every
# platform mutsu targets.
class Tm is repr('CStruct') {
    has int32 $.tm_sec;
    has int32 $.tm_min;
    has int32 $.tm_hour;
    has int32 $.tm_mday;
    has int32 $.tm_mon;
    has int32 $.tm_year;
    has int32 $.tm_wday;
    has int32 $.tm_yday;
    has int32 $.tm_isdst;
}

sub gmtime(CArray[int64] --> Pointer) is native { * }

my $epoch = CArray[int64].new;
$epoch[0] = 0;
my $p = gmtime($epoch);
ok $p.defined, 'gmtime returned a pointer';

# The epoch is 1970-01-01T00:00:00Z, a Thursday.
my $tm = nativecast(Tm, $p);
is $tm.tm_sec, 0, 'tm_sec at offset 0';
is $tm.tm_min, 0, 'tm_min at offset 4';
is $tm.tm_hour, 0, 'tm_hour at offset 8';
is $tm.tm_mday, 1, 'tm_mday at offset 12';
is $tm.tm_mon, 0, 'tm_mon at offset 16';
is $tm.tm_year, 70, 'tm_year at offset 20';
is $tm.tm_wday, 4, 'tm_wday at offset 24 (the epoch was a Thursday)';
is $tm.tm_yday, 0, 'tm_yday at offset 28';

# `struct lconv` (C89) begins with `char *decimal_point`, so it exercises a
# `Str` field (read through the pointer) and a pointer-shaped field.
class Lconv is repr('CStruct') {
    has Str $.decimal_point;
    has Pointer $.thousands_sep;
}

sub localeconv(--> Pointer) is native { * }

my $lc = nativecast(Lconv, localeconv());
is $lc.decimal_point, '.', 'a Str field reads through the char*';
ok $lc.thousands_sep.defined, 'a pointer field comes back as a Pointer';

# Casting the same address twice gives equal field reads: the cast is a
# reinterpretation, not a copy.
is nativecast(Tm, $p).tm_year, $tm.tm_year, 'nativecast reinterprets in place';
