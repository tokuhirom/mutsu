use Test;
use lib 't/lib';
use ExportedUnitClass;

plan 5;

#| A documented class.
class Doc is export {
    #| A documented method.
    method m { 1 }
    #| A documented attribute.
    has $.value;
}

is Doc.WHY.Str, 'A documented class.', 'leading class doc stays on an exported class';
is Doc.^find_method('m').WHY.Str, 'A documented method.', 'method keeps its own leading doc';
is Doc.^attributes.first(*.name eq '$!value').WHY.Str,
    'A documented attribute.', 'attribute keeps its own leading doc';

class TrailingDoc is export { } #= Trailing exported class.
is TrailingDoc.WHY.Str, 'Trailing exported class.', 'trailing class doc stays on exported class';

is ExportedUnitClass.WHY.Str, 'A documented exported unit class.',
    'unit class docs survive module loading';
