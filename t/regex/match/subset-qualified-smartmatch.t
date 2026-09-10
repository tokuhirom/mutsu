use v6;
use Test;

plan 6;

# A subset defaults to `our` scope: declared inside a package it is also
# reachable — and smartmatchable — by its qualified name (URI::Scheme).
class C {
    our subset Sch of Str where { .chars > 2 };
    our subset Rx of Str where /^ \w+ $/;
}

ok 'http' ~~ C::Sch, 'qualified subset smartmatch (block where)';
nok 'x' ~~ C::Sch, 'qualified subset rejects non-members';
ok 'abc' ~~ C::Rx, 'qualified subset smartmatch (regex where)';
nok '-x' ~~ C::Rx, 'qualified regex subset rejects non-members';

is C::Sch.^name, 'C::Sch', 'the qualified type object carries its name';

# A subset declared at top level keeps its bare-name behavior.
subset Top of Str where { .chars == 2 };
ok 'ab' ~~ Top, 'top-level subset still smartmatches by bare name';
