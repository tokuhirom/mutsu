use lib 't/lib';
use Test;

# From Algorithm::Treap 0.10.3 (`unit role Algorithm::Treap[::KeyT]; my enum
# TOrder is export <DESC ASC>;`): an enum exported from a parameterized role
# used to import as a bare Str, and the composition-time enum rejected the
# caller's `TOrder::ASC` with "Type check failed in binding to parameter".

use ParamRoleExportedEnum;

plan 7;

is TOrder.^name, 'TOrder', 'imported enum type has its own name';
is ASC.^name, 'TOrder', 'imported enum value belongs to the enum';
is DESC.value, 0, 'imported enum value keeps its value';

lives-ok { ParamRoleExportedEnum[Str].new(order-by => TOrder::ASC) },
    'imported enum value binds to the role-typed BUILD parameter';
is ParamRoleExportedEnum[Int].new(order-by => DESC).order-by, DESC,
    'the value round-trips through a typed attribute';
is ParamRoleExportedEnum[Str].new.order-by, ASC,
    'the role body sees the same enum type';
dies-ok { ParamRoleExportedEnum[Str].new(order-by => 'asc') },
    'a Str is still rejected by the enum-typed parameter';
