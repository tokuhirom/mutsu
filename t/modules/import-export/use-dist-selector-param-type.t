use Test;

# `use Foo:auth<...>` appends the dist selector to the module name. The
# compile-time pre-pass that collects a used module's type names (so an
# imported class may type a parameter) looked the module file up by that
# decorated name, missed it, and rejected the parameter with "Invalid
# typename". Reduced from Timezones::ZoneInfo, whose t/01-accuracy.t does
# `use Timezones::ZoneInfo::Time:auth<zef:guifa>` then `sub f(Time \a)`.

plan 3;

use lib 't/lib/DistSelectorUnitClass';
use Tm:auth<zef:someone>;

sub year-of(Tm \t) { t.year }
sub year-of-scalar(Tm $t --> Int) { $t.year }

is year-of(Tm.new), 2022, 'sigilless parameter typed by the imported class';
is year-of-scalar(Tm.new(year => 1999)), 1999, 'scalar parameter typed by the imported class';
isa-ok Tm.new, Tm, 'the class itself is imported';
