use v6;
use Test;

plan 6;

# In a unit that imported through a run-time `sub EXPORT` hook the keywords `True`,
# `False`, `Nil`, `Empty` and `Any` are the parser's shadowable terms; finalization
# carries them as built (#9047, ADR-15760 slice 2).

use lib 't/lib';
use ExportHookShadowsTermKeyword;

is True.Str, 'Tri(1)', 'the imported True shadows the keyword';
is False.Int, -1, 'and so does False';
is True.^name, 'Tri', 'the shadowed term is the imported type';
nok Nil.defined, 'Nil is unshadowed and keeps its value';
is Empty.elems, 0, 'Empty likewise';
nok Any.defined, 'Any likewise';
