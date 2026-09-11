use lib 't/lib';
use Test;

use EnumExportSpaceAndSlipFixture;

# A space after `is export` means that the following parenthesized expression
# is the enum value list, not the export trait's own argument list. A `slip`
# in that list is evaluated at runtime, but its literal names must still be
# known while parsing the statements that follow it.
enum SpacedExport is export (:10SPACED_A, :11SPACED_B);
enum SpacedSlip is export (:20LOCAL_A, slip <LOCAL_B LOCAL_C>);

plan 11;

is SPACED_A.value, 10, 'space after is export keeps the first enum value';
is SPACED_B.value, 11, 'space after is export keeps later enum values';

my $spaced-match = 'not matched';
given SPACED_B {
    when SPACED_B { $spaced-match = 'matched' }
}
is $spaced-match, 'matched', 'spaced export enum values are complete parse-time terms';

is LOCAL_A.value, 20, 'a dynamic enum keeps its explicit value';
is LOCAL_B.value, 21, 'a slipped enum value follows the explicit value';
is LOCAL_C.value, 22, 'later slipped enum values auto-increment';

my $local-match = 'not matched';
given LOCAL_B {
    when LOCAL_B { $local-match = 'matched' }
}
is $local-match, 'matched', 'slipped local values are known to the parser';

is IMPORTED_A.value, 30, 'an imported dynamic enum keeps its explicit value';
is IMPORTED_B.value, 31, 'an imported slipped enum value is registered';
is IMPORTED_C.value, 32, 'an imported slipped enum value auto-increments';

my $imported-match = 'not matched';
given IMPORTED_B {
    when IMPORTED_B { $imported-match = 'matched' }
}
is $imported-match, 'matched', 'imported slipped values are known to the parser';
