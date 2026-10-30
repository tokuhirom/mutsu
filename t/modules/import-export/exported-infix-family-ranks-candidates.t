# The candidates of an `infix:<op>` family a module re-exports through
# `sub EXPORT { Other::EXPORT::ALL:: }` are ranked by narrowness, like any other
# multi. Red re-exports its `infix:<==>` family that way; taking the first
# candidate that binds picked `Date()` for a DateTime operand where `DateTime()`
# is the narrower one.
use Test;
use lib 't/lib';

plan 5;

use ExportedInfixFamilyReexport;

my $dt = DateTime.new('2024-02-29T12:34:56Z');
my $d = Date.new('2024-02-29');
my $n = Node.new(tag => 'x');

is ($n == $dt), 'DateTime()', 'a DateTime variable picks the DateTime() candidate';
is ($n == $d), 'Date()', 'a Date variable picks the Date() candidate';
is ($n == DateTime.new('2024-02-29T12:34:56Z')), 'DateTime() ro', 'a DateTime literal picks the read-only one';
is ($n == Date.new('2024-02-29')), 'Date() ro', 'a Date literal picks the read-only one';

# A call no exported candidate accepts declines to the core operator.
is (1 == 1), True, 'core == still works';
