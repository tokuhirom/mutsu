unit module RelativeLookup;
use Test;

class C {}
class RelativeLookup::C {}

is C.^name, 'RelativeLookup::C', 'short name finds the sibling class';
is RelativeLookup::C.^name, 'RelativeLookup::RelativeLookup::C',
    'qualified name resolves relative to the unit package';
done-testing;
