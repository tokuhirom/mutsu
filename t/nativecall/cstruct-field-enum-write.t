use v6;
use Test;
use NativeCall;

plan 3;

# Writing an enum value into a CStruct field must store the enum's underlying
# integer. `to_int` had no Enum arm, so `.buffer_type = MYSQL_TYPE_DOUBLE`
# stored 0 (MYSQL_TYPE_DECIMAL) and every DBIish prepared-statement parameter
# was marshalled as a decimal string, failing with "Out of range value".

enum FieldKind ( FK_ZERO => 0, FK_DOUBLE => 5, FK_STRING => 254 );

class FieldBox is repr('CStruct') {
    has uint32 $.t is rw;
    has uint32 $.u is rw;
}

sub malloc(size_t --> Pointer) is native(Str) { * }
sub free(Pointer) is native(Str) { * }

my $mem = malloc(8);
my $box = nativecast(FieldBox, $mem);

$box.t = FK_DOUBLE;
$box.u = FK_STRING;
is $box.t, 5, 'an enum value written to a CStruct field stores its Int value';
is $box.u, 254, 'and a large member too';

$box.t = FK_ZERO;
is $box.t, 0, 'the zero member overwrites';

free($mem);
