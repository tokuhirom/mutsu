use v6;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;
use Test;

plan 17;

# A module body runs in the importer's env, so the companion markers its
# file-scope declarations write -- `__mutsu_constant_var::` for a `constant`,
# `__mutsu_type::` for a typed binding -- used to stay behind in every frame
# env of the importing program (ADR-0084 group 3, #7817). They now live in a
# per-package table, or leave with the binding they describe. None of that is
# visible from Raku; what is pinned here is that every reader of a marker
# still answers as before, and as rakudo does.

# The importer's own declarations under the same names, made before the load,
# must come back unchanged.
constant LABEL = 'importer-label';
my Int $counter = 10;

use ToplevelMarkersUnit;
use ToplevelMarkersBare;

# The unit module's own routines still see their constants and types.
ok ToplevelMarkersUnit::matches('xabcx'), 'a unit module routine interpolates its own constant into a regex';
nok ToplevelMarkersUnit::matches('xyz'), '... and the constant still matches literally';
is ToplevelMarkersUnit::version(), 3, 'a typed unit constant reads back';
is ToplevelMarkersUnit::label(), 'unit-label', 'a unit constant reads back';
is ToplevelMarkersUnit::eval-term(), 'unit-eval', 'EVAL inside the unit module sees its own constant as a term';
is ToplevelMarkersUnit::bump(), 1, 'a typed unit `my` assigns from the module routine';
throws-like { ToplevelMarkersUnit::bump-bad() }, X::TypeCheck::Assignment,
    'the typed unit `my` keeps its type check in the module routine';

# The importer keeps its own bindings and their markers.
is LABEL, 'importer-label', "the importer's own constant is restored";
is EVAL('LABEL'), 'importer-label', "EVAL in the importer sees the importer's constant";
$counter = 11;
is $counter, 11, "the importer's typed variable assigns";
throws-like { $counter = 'nope' }, X::TypeCheck::Assignment,
    "the importer's typed variable keeps its own type check";

# A unit module's private constants are not the importer's terms.
ok (try EVAL '$pat') ~~ Nil, "a unit module's `constant \$pat` is not visible to the importer";
ok (try EVAL 'EVAL-ONLY') ~~ Nil, "nor is its sigilless constant an importer term";

# A package-less module file: its class methods still see the file's
# constants, and a lexical shadow still hides the marker.
ok ToplevelMarkersBare.matches('axyzb'), 'a class method interpolates its file constant into a regex';
nok ToplevelMarkersBare.matches('abc'), '... literally';
is ToplevelMarkersBare.hidden, 7, "a class method reads its file's `my constant`";
ok ToplevelMarkersBare.shadowed('aqb'), 'a `my` shadowing the constant interpolates its own value';
