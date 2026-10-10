use Test;

# IP::Addr: `unit module IP::Addr::Const` lives in IP/Addr/Common.rakumod, so the
# declared package differs from the `use` name. The exported enum values must
# still import as enum values, not as barewords.

plan 4;

use lib 't/lib';
use EnumUnitModuleNameMismatch;

isa-ok unknown, FORM-KIND, 'exported enum value imports as the enum';
is cidr.value, 2, 'later value keeps its position';
my FORM-KIND $f = unknown;
is $f, 'unknown', 'typed container accepts the imported value';
is single.WHAT.^name, 'FORM-KIND', 'WHAT of the value';
