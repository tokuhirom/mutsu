use lib 't/lib';
use Test;

# From Date::Calendar::Gregorian (via Date::Names): an attribute default naming
# an enum member of the declaring module resolved to the bare string "keep" when
# `.new` was called from a method of another file.

use EnumAttrDefaultCrossModule;

plan 3;

class Caller { method make { EnumAttrDefaultCrossModule.new } }

is EnumAttrDefaultCrossModule.new.period, EnumAttrDefaultCrossModule::keep, 'top level';
my $o = Caller.new.make;
is $o.period, EnumAttrDefaultCrossModule::keep, 'from a method';
isa-ok $o.period, EnumAttrDefaultCrossModule::Period, 'enum value, not a Str';
